// Not needed in interpreter loop: func_obj rt p sf_s, arg_allocated_size i local_buf stack_buf pval alloca_size
// First cold var: sf
// Maybe move to stack frame: caller_ctx new_target flags
//
// Handle: ret_val

#if __has_attribute(preserve_none)
#define PRESERVE_NONE __attribute__((preserve_none))
#else
#warning "preserve_none not supported"
#define PRESERVE_NONE
#endif

#if __has_attribute(musttail)
#define MUSTTAIL __attribute__((musttail))
#else
#warning "must_tail not supported"
#define MUSTTAIL
#endif

#define TAIL_DISPATCH 0
#define TAIL_CALL_ARGS(pc) pc, sp, b, ctx, var_buf, arg_buf, var_refs, sf, ret_val, caller_ctx, this_obj, new_target, argc, argv, flags, local_buf
#define TAIL_CALL_PARAMS \
    const uint8_t *pc, \
    JSValue *sp, \
    JSFunctionBytecode *b, \
    JSContext *ctx, \
    JSValue *var_buf, \
    JSValue *arg_buf, \
    JSVarRef **var_refs, \
    JSStackFrame *sf, \
    JSValue ret_val, \
    JSContext *caller_ctx, \
    JSValueConst this_obj, \
    JSValueConst new_target, \
    int argc, \
    JSValue *argv, \
    int flags, \
    JSValue* local_buf

#if TAIL_DISPATCH
#define END_BRACE }
PRESERVE_NONE static JSValue jsci_label_exception(TAIL_CALL_PARAMS);
PRESERVE_NONE static JSValue jsci_label_done(TAIL_CALL_PARAMS);
PRESERVE_NONE static JSValue jsci_label_done_generator(TAIL_CALL_PARAMS);
typedef PRESERVE_NONE JSValue(*JSHandler)(TAIL_CALL_PARAMS);
extern JSHandler jsci_jump_table[];
#endif

/* argv[] is modified if (flags & JS_CALL_FLAG_COPY_ARGV) = 0. */
static JSValue JS_CallInternal(JSContext *caller_ctx, JSValueConst func_obj,
                               JSValueConst this_obj, JSValueConst new_target,
                               int argc, JSValue *argv, int flags)
{
    JSRuntime *rt = caller_ctx->rt;
    JSContext *ctx;
    JSObject *p;
    JSFunctionBytecode *b;
    JSStackFrame sf_s, *sf = &sf_s;
    const uint8_t *pc;
    int arg_allocated_size, i;
    JSValue *local_buf, *stack_buf, *var_buf, *arg_buf, *sp, ret_val = JS_UNDEFINED, *pval;
    JSVarRef **var_refs;
    size_t alloca_size;

#if TAIL_DISPATCH
#define HANDLER(op)          PRESERVE_NONE static JSValue handle_##op(TAIL_CALL_PARAMS)
#define HANDLER_FALLTHROUGH(op, op2)  \
    HANDLER(op) { MUSTTAIL return jsci_jump_table[op2](TAIL_CALL_ARGS(pc)); }
#define BREAK                MUSTTAIL return jsci_jump_table[*pc](TAIL_CALL_ARGS(pc+1))
#define DEFAULT
#define GOTO_LABEL(name)     MUSTTAIL return jsci_label_##name(TAIL_CALL_ARGS(pc))
#define LABEL_HANDLER(name)  PRESERVE_NONE static JSValue jsci_label_##name(TAIL_CALL_PARAMS)
#define BEGIN_HANDLERS \
    restart: return jsci_jump_table[*pc](TAIL_CALL_ARGS(pc+1)); \
    exception: return jsci_label_exception(TAIL_CALL_ARGS(pc)); \
    END_BRACE  /* end JS_CallInternal */

#elif !DIRECT_DISPATCH
#define SWITCH(pc)      switch (*pc++)
#define HANDLER(op)     case op:
#define HANDLER_FALLTHROUGH(op, op2)  case op:
#define DEFAULT         default:
#define BREAK           break
#define GOTO_LABEL(lb)  goto lb
#else
    static const void * const dispatch_table[256] = {
#define DEF(id, size, n_pop, n_push, f) && case_OP_ ## id,
#if SHORT_OPCODES
#define def(id, size, n_pop, n_push, f)
#else
#define def(id, size, n_pop, n_push, f) && case_default,
#endif
#include "quickjs-opcode.h"
        [ OP_COUNT ... 255 ] = &&case_default
    };
#define SWITCH(pc)      goto *dispatch_table[*pc++];
#define HANDLER(op)     case_ ## op:
#define HANDLER_FALLTHROUGH(op, op2)  case_ ## op:
#define DEFAULT         case_default:
#define BREAK           SWITCH(pc)
#define GOTO_LABEL(lb)  goto lb
#endif

    if (js_poll_interrupts(caller_ctx))
        return JS_EXCEPTION;
    if (unlikely(JS_VALUE_GET_TAG(func_obj) != JS_TAG_OBJECT)) {
        if (flags & JS_CALL_FLAG_GENERATOR) {
            JSAsyncFunctionState *s = JS_VALUE_GET_PTR(func_obj);
            /* func_obj get contains a pointer to JSFuncAsyncState */
            /* the stack frame is already allocated */
            sf = &s->frame;
            p = JS_VALUE_GET_OBJ(sf->cur_func);
            b = p->u.func.function_bytecode;
            ctx = b->realm;
            var_refs = p->u.func.var_refs;
            local_buf = arg_buf = sf->arg_buf;
            var_buf = sf->var_buf;
            stack_buf = sf->var_buf + b->var_count;
            sp = sf->cur_sp;
            sf->cur_sp = NULL; /* cur_sp is NULL if the function is running */
            pc = sf->cur_pc;
            sf->prev_frame = rt->current_stack_frame;
            rt->current_stack_frame = sf;
            if (s->throw_flag)
                goto exception;
            else
                goto restart;
        } else {
            goto not_a_function;
        }
    }
    p = JS_VALUE_GET_OBJ(func_obj);
    if (unlikely(p->class_id != JS_CLASS_BYTECODE_FUNCTION)) {
        JSClassCall *call_func;
        call_func = rt->class_array[p->class_id].call;
        if (!call_func) {
        not_a_function:
            return JS_ThrowTypeError(caller_ctx, "not a function");
        }
        return call_func(caller_ctx, func_obj, this_obj, argc,
                         (JSValueConst *)argv, flags);
    }
    b = p->u.func.function_bytecode;

    if (unlikely(argc < b->arg_count || (flags & JS_CALL_FLAG_COPY_ARGV))) {
        arg_allocated_size = b->arg_count;
    } else {
        arg_allocated_size = 0;
    }

    alloca_size = sizeof(JSValue) * (arg_allocated_size + b->var_count +
                                     b->stack_size);
    if (js_check_stack_overflow(rt, alloca_size))
        return JS_ThrowStackOverflow(caller_ctx);

    sf->js_mode = b->js_mode;
    arg_buf = argv;
    sf->arg_count = argc;
    sf->cur_func = (JSValue)func_obj;
    init_list_head(&sf->var_ref_list);
    var_refs = p->u.func.var_refs;

    local_buf = alloca(alloca_size);
    if (unlikely(arg_allocated_size)) {
        int n = min_int(argc, b->arg_count);
        arg_buf = local_buf;
        for(i = 0; i < n; i++)
            arg_buf[i] = JS_DupValue(caller_ctx, argv[i]);
        for(; i < b->arg_count; i++)
            arg_buf[i] = JS_UNDEFINED;
        sf->arg_count = b->arg_count;
    }
    var_buf = local_buf + arg_allocated_size;
    sf->var_buf = var_buf;
    sf->arg_buf = arg_buf;

    for(i = 0; i < b->var_count; i++)
        var_buf[i] = JS_UNDEFINED;

    stack_buf = var_buf + b->var_count;
    sp = stack_buf;
    pc = b->byte_code_buf;
    sf->prev_frame = rt->current_stack_frame;
    rt->current_stack_frame = sf;
    ctx = b->realm; /* set the current realm */
    goto restart;

/*****************************************************************************/

#if TAIL_DISPATCH
BEGIN_HANDLERS
#else
restart:
    for(;;) {
        SWITCH(pc) {
#endif

        HANDLER(OP_push_i32) {
            *sp++ = JS_NewInt32(ctx, get_u32(pc));
            pc += 4;
            BREAK;
        }
        HANDLER(OP_push_bigint_i32) {
            *sp++ = __JS_NewShortBigInt(ctx, (int)get_u32(pc));
            pc += 4;
            BREAK;
        }
        HANDLER(OP_push_const) {
            *sp++ = JS_DupValue(ctx, b->cpool[get_u32(pc)]);
            pc += 4;
            BREAK;
        }
#if SHORT_OPCODES
        HANDLER(OP_push_minus1) { *sp++ = JS_NewInt32(ctx, -1); BREAK; }
        HANDLER(OP_push_0) { *sp++ = JS_NewInt32(ctx, 0); BREAK; }
        HANDLER(OP_push_1) { *sp++ = JS_NewInt32(ctx, 1); BREAK; }
        HANDLER(OP_push_2) { *sp++ = JS_NewInt32(ctx, 2); BREAK; }
        HANDLER(OP_push_3) { *sp++ = JS_NewInt32(ctx, 3); BREAK; }
        HANDLER(OP_push_4) { *sp++ = JS_NewInt32(ctx, 4); BREAK; }
        HANDLER(OP_push_5) { *sp++ = JS_NewInt32(ctx, 5); BREAK; }
        HANDLER(OP_push_6) { *sp++ = JS_NewInt32(ctx, 6); BREAK; }
        HANDLER(OP_push_7) { *sp++ = JS_NewInt32(ctx, 7); BREAK; }
        HANDLER(OP_push_i8) {
            *sp++ = JS_NewInt32(ctx, get_i8(pc));
            pc += 1;
            BREAK;
        }
        HANDLER(OP_push_i16) {
            *sp++ = JS_NewInt32(ctx, get_i16(pc));
            pc += 2;
            BREAK;
        }
        HANDLER(OP_push_const8) {
            *sp++ = JS_DupValue(ctx, b->cpool[*pc++]);
            BREAK;
        }
        HANDLER(OP_fclosure8) {
            *sp++ = js_closure(ctx, JS_DupValue(ctx, b->cpool[*pc++]), var_refs, sf);
            if (unlikely(JS_IsException(sp[-1])))
                GOTO_LABEL(exception);
            BREAK;
        }
        HANDLER(OP_push_empty_string) {
            *sp++ = JS_AtomToString(ctx, JS_ATOM_empty_string);
            BREAK;
        }
        HANDLER(OP_get_length) {
            JSValue val;
            sf->cur_pc = pc;
            val = JS_GetProperty(ctx, sp[-1], JS_ATOM_length);
            if (unlikely(JS_IsException(val)))
                GOTO_LABEL(exception);
            JS_FreeValue(ctx, sp[-1]);
            sp[-1] = val;
            BREAK;
        }
#endif
        HANDLER(OP_push_atom_value) {
            *sp++ = JS_AtomToValue(ctx, get_u32(pc));
            pc += 4;
            BREAK;
        }
        HANDLER(OP_undefined) {
            *sp++ = JS_UNDEFINED;
            BREAK;
        }
        HANDLER(OP_null) {
            *sp++ = JS_NULL;
            BREAK;
        }
        HANDLER(OP_push_this)
            /* OP_push_this is only called at the start of a function */
            {
                JSValue val;
                if (!(b->js_mode & JS_MODE_STRICT)) {
                    uint32_t tag = JS_VALUE_GET_TAG(this_obj);
                    if (likely(tag == JS_TAG_OBJECT))
                        goto normal_this;
                    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED) {
                        val = JS_DupValue(ctx, ctx->global_obj);
                    } else {
                        val = JS_ToObject(ctx, this_obj);
                        if (JS_IsException(val))
                            GOTO_LABEL(exception);
                    }
                } else {
                normal_this:
                    val = JS_DupValue(ctx, this_obj);
                }
                *sp++ = val;
                BREAK;
            }
        HANDLER(OP_push_false) {
            *sp++ = JS_FALSE;
            BREAK;
        }
        HANDLER(OP_push_true) {
            *sp++ = JS_TRUE;
            BREAK;
        }
        HANDLER(OP_object) {
            *sp++ = JS_NewObject(ctx);
            if (unlikely(JS_IsException(sp[-1])))
                GOTO_LABEL(exception);
            BREAK;
        }
        HANDLER(OP_special_object)
            {
                int arg = *pc++;
                switch(arg) {
                case OP_SPECIAL_OBJECT_ARGUMENTS:
                    *sp++ = js_build_arguments(ctx, argc, (JSValueConst *)argv);
                    if (unlikely(JS_IsException(sp[-1])))
                        GOTO_LABEL(exception);
                    break;
                case OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS:
                    *sp++ = js_build_mapped_arguments(ctx, argc, (JSValueConst *)argv,
                                                      sf, min_int(argc, b->arg_count));
                    if (unlikely(JS_IsException(sp[-1])))
                        GOTO_LABEL(exception);
                    break;
                case OP_SPECIAL_OBJECT_THIS_FUNC:
                    *sp++ = JS_DupValue(ctx, sf->cur_func);
                    break;
                case OP_SPECIAL_OBJECT_NEW_TARGET:
                    *sp++ = JS_DupValue(ctx, new_target);
                    break;
                case OP_SPECIAL_OBJECT_HOME_OBJECT:
                    {
                        JSObject *p = JS_VALUE_GET_OBJ(sf->cur_func);
                        JSObject *p1;
                        p1 = p->u.func.home_object;
                        if (unlikely(!p1))
                            *sp++ = JS_UNDEFINED;
                        else
                            *sp++ = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, p1));
                    }
                    break;
                case OP_SPECIAL_OBJECT_VAR_OBJECT:
                    *sp++ = JS_NewObjectProto(ctx, JS_NULL);
                    if (unlikely(JS_IsException(sp[-1])))
                        GOTO_LABEL(exception);
                    break;
                case OP_SPECIAL_OBJECT_IMPORT_META:
                    *sp++ = js_import_meta(ctx);
                    if (unlikely(JS_IsException(sp[-1])))
                        GOTO_LABEL(exception);
                    break;
                default:
                    abort();
                }
                BREAK;
            }
        HANDLER(OP_rest)
            {
                int first = get_u16(pc);
                pc += 2;
                *sp++ = js_build_rest(ctx, first, argc, (JSValueConst *)argv);
                if (unlikely(JS_IsException(sp[-1])))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_drop) {
            JS_FreeValue(ctx, sp[-1]);
            sp--;
            BREAK;
        }
        HANDLER(OP_nip) {
            JS_FreeValue(ctx, sp[-2]);
            sp[-2] = sp[-1];
            sp--;
            BREAK;
        }
        HANDLER(OP_nip1) { /* a b c -> b c */
            JS_FreeValue(ctx, sp[-3]);
            sp[-3] = sp[-2];
            sp[-2] = sp[-1];
            sp--;
            BREAK;
        }
        HANDLER(OP_dup) {
            sp[0] = JS_DupValue(ctx, sp[-1]);
            sp++;
            BREAK;
        }
        HANDLER(OP_dup2) { /* a b -> a b a b */
            sp[0] = JS_DupValue(ctx, sp[-2]);
            sp[1] = JS_DupValue(ctx, sp[-1]);
            sp += 2;
            BREAK;
        }
        HANDLER(OP_dup3) { /* a b c -> a b c a b c */
            sp[0] = JS_DupValue(ctx, sp[-3]);
            sp[1] = JS_DupValue(ctx, sp[-2]);
            sp[2] = JS_DupValue(ctx, sp[-1]);
            sp += 3;
            BREAK;
        }
        HANDLER(OP_dup1) { /* a b -> a a b */
            sp[0] = sp[-1];
            sp[-1] = JS_DupValue(ctx, sp[-2]);
            sp++;
            BREAK;
        }
        HANDLER(OP_insert2) { /* obj a -> a obj a (dup_x1) */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        }
        HANDLER(OP_insert3) { /* obj prop a -> a obj prop a (dup_x2) */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        }
        HANDLER(OP_insert4) { /* this obj prop a -> a this obj prop a */
            sp[0] = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = sp[-4];
            sp[-4] = JS_DupValue(ctx, sp[0]);
            sp++;
            BREAK;
        }
        HANDLER(OP_perm3) { /* obj a b -> a obj b (213) */
            JSValue tmp;
            tmp = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = tmp;
            BREAK;
        }
        HANDLER(OP_rot3l) { /* x a b -> a b x (231) */
            JSValue tmp;
            tmp = sp[-3];
            sp[-3] = sp[-2];
            sp[-2] = sp[-1];
            sp[-1] = tmp;
            BREAK;
        }
        HANDLER(OP_rot4l) { /* x a b c -> a b c x */
            JSValue tmp;
            tmp = sp[-4];
            sp[-4] = sp[-3];
            sp[-3] = sp[-2];
            sp[-2] = sp[-1];
            sp[-1] = tmp;
            BREAK;
        }
        HANDLER(OP_rot5l) { /* x a b c d -> a b c d x */
            JSValue tmp;
            tmp = sp[-5];
            sp[-5] = sp[-4];
            sp[-4] = sp[-3];
            sp[-3] = sp[-2];
            sp[-2] = sp[-1];
            sp[-1] = tmp;
            BREAK;
        }
        HANDLER(OP_rot3r) { /* a b x -> x a b (312) */
            JSValue tmp;
            tmp = sp[-1];
            sp[-1] = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = tmp;
            BREAK;
        }
        HANDLER(OP_perm4) { /* obj prop a b -> a obj prop b */
            JSValue tmp;
            tmp = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = sp[-4];
            sp[-4] = tmp;
            BREAK;
        }
        HANDLER(OP_perm5) { /* this obj prop a b -> a this obj prop b */
            JSValue tmp;
            tmp = sp[-2];
            sp[-2] = sp[-3];
            sp[-3] = sp[-4];
            sp[-4] = sp[-5];
            sp[-5] = tmp;
            BREAK;
        }
        HANDLER(OP_swap) { /* a b -> b a */
            JSValue tmp;
            tmp = sp[-2];
            sp[-2] = sp[-1];
            sp[-1] = tmp;
            BREAK;
        }
        HANDLER(OP_swap2) { /* a b c d -> c d a b */
            JSValue tmp1, tmp2;
            tmp1 = sp[-4];
            tmp2 = sp[-3];
            sp[-4] = sp[-2];
            sp[-3] = sp[-1];
            sp[-2] = tmp1;
            sp[-1] = tmp2;
            BREAK;
        }

        HANDLER(OP_fclosure)
            {
                JSValue bfunc = JS_DupValue(ctx, b->cpool[get_u32(pc)]);
                pc += 4;
                *sp++ = js_closure(ctx, bfunc, var_refs, sf);
                if (unlikely(JS_IsException(sp[-1])))
                    GOTO_LABEL(exception);
                BREAK;
            }
#if SHORT_OPCODES
        HANDLER_FALLTHROUGH(OP_call0, OP_call3)
        HANDLER_FALLTHROUGH(OP_call1, OP_call3)
        HANDLER_FALLTHROUGH(OP_call2, OP_call3)
        HANDLER(OP_call3)
          {
            int opcode_ = pc[-1];
            int call_argc = opcode_ - OP_call0;
            int i;
            JSValue *call_argv = sp - call_argc;
            sf->cur_pc = pc;
            ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                      JS_UNDEFINED, call_argc, call_argv, 0);
            if (unlikely(JS_IsException(ret_val)))
                GOTO_LABEL(exception);
            for(i = -1; i < call_argc; i++)
                JS_FreeValue(ctx, call_argv[i]);
            sp -= call_argc + 1;
            *sp++ = ret_val;
            BREAK;
          }
#endif
        HANDLER(OP_call)
            {
                int i;
                int call_argc = get_u16(pc);
                JSValue *call_argv = sp - call_argc;
                pc += 2;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                for(i = -1; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 1;
                *sp++ = ret_val;
                BREAK;
            }
        HANDLER(OP_tail_call)
            {
                int call_argc = get_u16(pc);
                JSValue *call_argv = sp - call_argc;
                pc += 2;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                GOTO_LABEL(done);
            }
        HANDLER(OP_call_constructor)
            {
                int i;
                int call_argc = get_u16(pc);
                JSValue *call_argv = sp - call_argc;
                pc += 2;
                sf->cur_pc = pc;
                ret_val = JS_CallConstructorInternal(ctx, call_argv[-2],
                                                     call_argv[-1],
                                                     call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                for(i = -2; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 2;
                *sp++ = ret_val;
                BREAK;
            }
        HANDLER(OP_call_method)
            {
                int i;
                int call_argc = get_u16(pc);
                JSValue *call_argv = sp - call_argc;
                pc += 2;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], call_argv[-2],
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                for(i = -2; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 2;
                *sp++ = ret_val;
                BREAK;
            }
        HANDLER(OP_tail_call_method)
            {
                int call_argc = get_u16(pc);
                JSValue *call_argv = sp - call_argc;
                pc += 2;
                sf->cur_pc = pc;
                ret_val = JS_CallInternal(ctx, call_argv[-1], call_argv[-2],
                                          JS_UNDEFINED, call_argc, call_argv, 0);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                GOTO_LABEL(done);
            }
        HANDLER(OP_array_from)
            {
                int i, ret;
                int call_argc = get_u16(pc);
                JSValue *call_argv;
                pc += 2;
                ret_val = JS_NewArray(ctx);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                call_argv = sp - call_argc;
                for(i = 0; i < call_argc; i++) {
                    ret = JS_DefinePropertyValue(ctx, ret_val, __JS_AtomFromUInt32(i), call_argv[i],
                                                 JS_PROP_C_W_E | JS_PROP_THROW);
                    call_argv[i] = JS_UNDEFINED;
                    if (ret < 0) {
                        JS_FreeValue(ctx, ret_val);
                        GOTO_LABEL(exception);
                    }
                }
                sp -= call_argc;
                *sp++ = ret_val;
                BREAK;
            }

        HANDLER(OP_apply)
            {
                int magic;
                magic = get_u16(pc);
                pc += 2;
                sf->cur_pc = pc;

                ret_val = js_function_apply(ctx, sp[-3], 2, (JSValueConst *)&sp[-2], magic);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 3;
                *sp++ = ret_val;
                BREAK;
            }
        HANDLER(OP_return) {
            ret_val = *--sp;
            GOTO_LABEL(done);
        }
        HANDLER(OP_return_undef) {
            ret_val = JS_UNDEFINED;
            GOTO_LABEL(done);
        }

        HANDLER(OP_check_ctor_return) {
            /* return TRUE if 'this' should be returned */
            if (!JS_IsObject(sp[-1])) {
                if (!JS_IsUndefined(sp[-1])) {
                    JS_ThrowTypeError(caller_ctx, "derived class constructor must return an object or undefined");
                    GOTO_LABEL(exception);
                }
                sp[0] = JS_TRUE;
            } else {
                sp[0] = JS_FALSE;
            }
            sp++;
            BREAK;
        }
        HANDLER(OP_check_ctor) {
            if (JS_IsUndefined(new_target)) {
                JS_ThrowTypeError(ctx, "class constructors must be invoked with 'new'");
                GOTO_LABEL(exception);
            }
            BREAK;
        }
        HANDLER(OP_init_ctor)
            {
                JSValue super, ret;
                sf->cur_pc = pc;
                if (JS_IsUndefined(new_target)) {
                    JS_ThrowTypeError(ctx, "class constructors must be invoked with 'new'");
                    GOTO_LABEL(exception);
                }
                JSValue func_obj_ = sf->cur_func;
                super = JS_GetPrototype(ctx, func_obj_);
                if (JS_IsException(super))
                    GOTO_LABEL(exception);
                ret = JS_CallConstructor2(ctx, super, new_target, argc, (JSValueConst *)argv);
                JS_FreeValue(ctx, super);
                if (JS_IsException(ret))
                    GOTO_LABEL(exception);
                *sp++ = ret;
                BREAK;
            }
        HANDLER(OP_check_brand)
            {
                int ret = JS_CheckBrand(ctx, sp[-2], sp[-1]);
                if (ret < 0)
                    GOTO_LABEL(exception);
                if (!ret) {
                    JS_ThrowTypeError(ctx, "invalid brand on object");
                    GOTO_LABEL(exception);
                }
                BREAK;
            }
        HANDLER(OP_add_brand) {
            if (JS_AddBrand(ctx, sp[-2], sp[-1]) < 0)
                GOTO_LABEL(exception);
            JS_FreeValue(ctx, sp[-2]);
            JS_FreeValue(ctx, sp[-1]);
            sp -= 2;
            BREAK;
        }

        HANDLER(OP_throw) {
            JS_Throw(ctx, *--sp);
            GOTO_LABEL(exception);
        }

        HANDLER(OP_throw_error)
#define JS_THROW_VAR_RO             0
#define JS_THROW_VAR_REDECL         1
#define JS_THROW_VAR_UNINITIALIZED  2
#define JS_THROW_ERROR_DELETE_SUPER   3
#define JS_THROW_ERROR_ITERATOR_THROW 4
            {
                JSAtom atom;
                int type;
                atom = get_u32(pc);
                type = pc[4];
                pc += 5;
                if (type == JS_THROW_VAR_RO)
                    JS_ThrowTypeErrorReadOnly(ctx, JS_PROP_THROW, atom);
                else
                if (type == JS_THROW_VAR_REDECL)
                    JS_ThrowSyntaxErrorVarRedeclaration(ctx, atom);
                else
                if (type == JS_THROW_VAR_UNINITIALIZED)
                    JS_ThrowReferenceErrorUninitialized(ctx, atom);
                else
                if (type == JS_THROW_ERROR_DELETE_SUPER)
                    JS_ThrowReferenceError(ctx, "unsupported reference to 'super'");
                else
                if (type == JS_THROW_ERROR_ITERATOR_THROW)
                    JS_ThrowTypeError(ctx, "iterator does not have a throw method");
                else
                    JS_ThrowInternalError(ctx, "invalid throw var type %d", type);
                GOTO_LABEL(exception);
            }

        HANDLER(OP_eval)
            {
                JSValueConst obj;
                int scope_idx;
                int i;
                int call_argc = get_u16(pc);
                JSValue* call_argv = sp - call_argc;
                scope_idx = get_u16(pc + 2) + ARG_SCOPE_END;
                pc += 4;
                sf->cur_pc = pc;
                if (js_same_value(ctx, call_argv[-1], ctx->eval_obj)) {
                    if (call_argc >= 1)
                        obj = call_argv[0];
                    else
                        obj = JS_UNDEFINED;
                    ret_val = JS_EvalObject(ctx, JS_UNDEFINED, obj,
                                            JS_EVAL_TYPE_DIRECT, scope_idx);
                } else {
                    ret_val = JS_CallInternal(ctx, call_argv[-1], JS_UNDEFINED,
                                              JS_UNDEFINED, call_argc, call_argv, 0);
                }
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                for(i = -1; i < call_argc; i++)
                    JS_FreeValue(ctx, call_argv[i]);
                sp -= call_argc + 1;
                *sp++ = ret_val;
                BREAK;
            }
            /* could merge with OP_apply */
        HANDLER(OP_apply_eval)
            {
                int scope_idx;
                uint32_t len;
                JSValue *tab;
                JSValueConst obj;

                scope_idx = get_u16(pc) + ARG_SCOPE_END;
                pc += 2;
                sf->cur_pc = pc;
                tab = build_arg_list(ctx, &len, sp[-1]);
                if (!tab)
                    GOTO_LABEL(exception);
                if (js_same_value(ctx, sp[-2], ctx->eval_obj)) {
                    if (len >= 1)
                        obj = tab[0];
                    else
                        obj = JS_UNDEFINED;
                    ret_val = JS_EvalObject(ctx, JS_UNDEFINED, obj,
                                            JS_EVAL_TYPE_DIRECT, scope_idx);
                } else {
                    ret_val = JS_Call(ctx, sp[-2], JS_UNDEFINED, len,
                                      (JSValueConst *)tab);
                }
                free_arg_list(ctx, tab, len);
                if (unlikely(JS_IsException(ret_val)))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 2;
                *sp++ = ret_val;
                BREAK;
            }

        HANDLER(OP_regexp)
            {
                sp[-2] = js_regexp_constructor_internal(ctx, JS_UNDEFINED,
                                                        sp[-2], sp[-1]);
                sp--;
                BREAK;
            }

        HANDLER(OP_get_super)
            {
                JSValue proto;
                sf->cur_pc = pc;
                proto = JS_GetPrototype(ctx, sp[-1]);
                if (JS_IsException(proto))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = proto;
                BREAK;
            }

        HANDLER(OP_import)
            {
                JSValue val;
                sf->cur_pc = pc;
                val = js_dynamic_import(ctx, sp[-2], sp[-1]);
                if (JS_IsException(val))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-1]);
                sp--;
                sp[-1] = val;
                BREAK;
            }

        HANDLER(OP_check_var)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                ret = JS_CheckGlobalVar(ctx, atom);
                if (ret < 0)
                    GOTO_LABEL(exception);
                *sp++ = JS_NewBool(ctx, ret);
                BREAK;
            }

        HANDLER(OP_get_var_undef)
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                val = JS_GetGlobalVar(ctx, atom, 0);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                *sp++ = val;
                BREAK;
            }
        HANDLER(OP_get_var)
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                val = JS_GetGlobalVar(ctx, atom, 1);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                *sp++ = val;
                BREAK;
            }

        HANDLER(OP_put_var)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                ret = JS_SetGlobalVar(ctx, atom, sp[-1], 0);
                sp--;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_put_var_init)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                ret = JS_SetGlobalVar(ctx, atom, sp[-1], 1);
                sp--;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_put_var_strict)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                /* sp[-2] is JS_TRUE or JS_FALSE */
                if (unlikely(!JS_VALUE_GET_INT(sp[-2]))) {
                    JS_ThrowReferenceErrorNotDefined(ctx, atom);
                    GOTO_LABEL(exception);
                }
                ret = JS_SetGlobalVar(ctx, atom, sp[-1], 2);
                sp -= 2;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_check_define_var)
            {
                JSAtom atom;
                int pc_flags;
                atom = get_u32(pc);
                pc_flags = pc[4];
                pc += 5;
                sf->cur_pc = pc;
                if (JS_CheckDefineGlobalVar(ctx, atom, pc_flags))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_define_var)
            {
                JSAtom atom;
                int pc_flags;
                atom = get_u32(pc);
                pc_flags = pc[4];
                pc += 5;
                sf->cur_pc = pc;
                if (JS_DefineGlobalVar(ctx, atom, pc_flags))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_define_func)
            {
                JSAtom atom;
                int pc_flags;
                atom = get_u32(pc);
                pc_flags = pc[4];
                pc += 5;
                sf->cur_pc = pc;
                if (JS_DefineGlobalFunction(ctx, atom, sp[-1], pc_flags))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp--;
                BREAK;
            }

        HANDLER(OP_get_loc)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                sp[0] = JS_DupValue(ctx, var_buf[idx]);
                sp++;
                BREAK;
            }
        HANDLER(OP_put_loc)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_set_loc)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], JS_DupValue(ctx, sp[-1]));
                BREAK;
            }
        HANDLER(OP_get_arg)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                sp[0] = JS_DupValue(ctx, arg_buf[idx]);
                sp++;
                BREAK;
            }
        HANDLER(OP_put_arg)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &arg_buf[idx], sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_set_arg)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &arg_buf[idx], JS_DupValue(ctx, sp[-1]));
                BREAK;
            }

#if SHORT_OPCODES
        HANDLER(OP_get_loc8) { *sp++ = JS_DupValue(ctx, var_buf[*pc++]); BREAK; }
        HANDLER(OP_put_loc8) { set_value(ctx, &var_buf[*pc++], *--sp); BREAK; }
        HANDLER(OP_set_loc8) { set_value(ctx, &var_buf[*pc++], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_get_loc0) { *sp++ = JS_DupValue(ctx, var_buf[0]); BREAK; }
        HANDLER(OP_get_loc1) { *sp++ = JS_DupValue(ctx, var_buf[1]); BREAK; }
        HANDLER(OP_get_loc2) { *sp++ = JS_DupValue(ctx, var_buf[2]); BREAK; }
        HANDLER(OP_get_loc3) { *sp++ = JS_DupValue(ctx, var_buf[3]); BREAK; }
        HANDLER(OP_put_loc0) { set_value(ctx, &var_buf[0], *--sp); BREAK; }
        HANDLER(OP_put_loc1) { set_value(ctx, &var_buf[1], *--sp); BREAK; }
        HANDLER(OP_put_loc2) { set_value(ctx, &var_buf[2], *--sp); BREAK; }
        HANDLER(OP_put_loc3) { set_value(ctx, &var_buf[3], *--sp); BREAK; }
        HANDLER(OP_set_loc0) { set_value(ctx, &var_buf[0], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_loc1) { set_value(ctx, &var_buf[1], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_loc2) { set_value(ctx, &var_buf[2], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_loc3) { set_value(ctx, &var_buf[3], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_get_arg0) { *sp++ = JS_DupValue(ctx, arg_buf[0]); BREAK; }
        HANDLER(OP_get_arg1) { *sp++ = JS_DupValue(ctx, arg_buf[1]); BREAK; }
        HANDLER(OP_get_arg2) { *sp++ = JS_DupValue(ctx, arg_buf[2]); BREAK; }
        HANDLER(OP_get_arg3) { *sp++ = JS_DupValue(ctx, arg_buf[3]); BREAK; }
        HANDLER(OP_put_arg0) { set_value(ctx, &arg_buf[0], *--sp); BREAK; }
        HANDLER(OP_put_arg1) { set_value(ctx, &arg_buf[1], *--sp); BREAK; }
        HANDLER(OP_put_arg2) { set_value(ctx, &arg_buf[2], *--sp); BREAK; }
        HANDLER(OP_put_arg3) { set_value(ctx, &arg_buf[3], *--sp); BREAK; }
        HANDLER(OP_set_arg0) { set_value(ctx, &arg_buf[0], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_arg1) { set_value(ctx, &arg_buf[1], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_arg2) { set_value(ctx, &arg_buf[2], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_arg3) { set_value(ctx, &arg_buf[3], JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_get_var_ref0) { *sp++ = JS_DupValue(ctx, *var_refs[0]->pvalue); BREAK; }
        HANDLER(OP_get_var_ref1) { *sp++ = JS_DupValue(ctx, *var_refs[1]->pvalue); BREAK; }
        HANDLER(OP_get_var_ref2) { *sp++ = JS_DupValue(ctx, *var_refs[2]->pvalue); BREAK; }
        HANDLER(OP_get_var_ref3) { *sp++ = JS_DupValue(ctx, *var_refs[3]->pvalue); BREAK; }
        HANDLER(OP_put_var_ref0) { set_value(ctx, var_refs[0]->pvalue, *--sp); BREAK; }
        HANDLER(OP_put_var_ref1) { set_value(ctx, var_refs[1]->pvalue, *--sp); BREAK; }
        HANDLER(OP_put_var_ref2) { set_value(ctx, var_refs[2]->pvalue, *--sp); BREAK; }
        HANDLER(OP_put_var_ref3) { set_value(ctx, var_refs[3]->pvalue, *--sp); BREAK; }
        HANDLER(OP_set_var_ref0) { set_value(ctx, var_refs[0]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_var_ref1) { set_value(ctx, var_refs[1]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_var_ref2) { set_value(ctx, var_refs[2]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK; }
        HANDLER(OP_set_var_ref3) { set_value(ctx, var_refs[3]->pvalue, JS_DupValue(ctx, sp[-1])); BREAK; }
#endif

        HANDLER(OP_get_var_ref)
            {
                int idx;
                JSValue val;
                idx = get_u16(pc);
                pc += 2;
                val = *var_refs[idx]->pvalue;
                sp[0] = JS_DupValue(ctx, val);
                sp++;
                BREAK;
            }
        HANDLER(OP_put_var_ref)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_set_var_ref)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, var_refs[idx]->pvalue, JS_DupValue(ctx, sp[-1]));
                BREAK;
            }
        HANDLER(OP_get_var_ref_check)
            {
                int idx;
                JSValue val;
                idx = get_u16(pc);
                pc += 2;
                val = *var_refs[idx]->pvalue;
                if (unlikely(JS_IsUninitialized(val))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    GOTO_LABEL(exception);
                }
                sp[0] = JS_DupValue(ctx, val);
                sp++;
                BREAK;
            }
        HANDLER(OP_put_var_ref_check)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(*var_refs[idx]->pvalue))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    GOTO_LABEL(exception);
                }
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_put_var_ref_check_init)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(!JS_IsUninitialized(*var_refs[idx]->pvalue))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, TRUE);
                    GOTO_LABEL(exception);
                }
                set_value(ctx, var_refs[idx]->pvalue, sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_set_loc_uninitialized)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                set_value(ctx, &var_buf[idx], JS_UNINITIALIZED);
                BREAK;
            }
        HANDLER(OP_get_loc_check)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, FALSE);
                    GOTO_LABEL(exception);
                }
                sp[0] = JS_DupValue(ctx, var_buf[idx]);
                sp++;
                BREAK;
            }
        HANDLER(OP_get_loc_checkthis)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceErrorUninitialized2(caller_ctx, b, idx, FALSE);
                    GOTO_LABEL(exception);
                }
                sp[0] = JS_DupValue(ctx, var_buf[idx]);
                sp++;
                BREAK;
            }
        HANDLER(OP_put_loc_check)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceErrorUninitialized2(ctx, b, idx, FALSE);
                    GOTO_LABEL(exception);
                }
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_put_loc_check_init)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                if (unlikely(!JS_IsUninitialized(var_buf[idx]))) {
                    JS_ThrowReferenceError(ctx, "'this' can be initialized only once");
                    GOTO_LABEL(exception);
                }
                set_value(ctx, &var_buf[idx], sp[-1]);
                sp--;
                BREAK;
            }
        HANDLER(OP_close_loc)
            {
                int idx;
                idx = get_u16(pc);
                pc += 2;
                close_lexical_var(ctx, sf, idx);
                BREAK;
            }

        HANDLER_FALLTHROUGH(OP_make_loc_ref, OP_make_var_ref_ref)
        HANDLER_FALLTHROUGH(OP_make_arg_ref, OP_make_var_ref_ref)
        HANDLER(OP_make_var_ref_ref)
            {
                int opcode_ = pc[-1];
                JSVarRef *var_ref;
                JSProperty *pr;
                JSAtom atom;
                int idx;
                atom = get_u32(pc);
                idx = get_u16(pc + 4);
                pc += 6;
                *sp++ = JS_NewObjectProto(ctx, JS_NULL);
                if (unlikely(JS_IsException(sp[-1])))
                    GOTO_LABEL(exception);
                if (opcode_ == OP_make_var_ref_ref) {
                    var_ref = var_refs[idx];
                    var_ref->header.ref_count++;
                } else {
                    var_ref = get_var_ref(ctx, sf, idx, opcode_ == OP_make_arg_ref);
                    if (!var_ref)
                        GOTO_LABEL(exception);
                }
                pr = add_property(ctx, JS_VALUE_GET_OBJ(sp[-1]), atom,
                                  JS_PROP_WRITABLE | JS_PROP_VARREF);
                if (!pr) {
                    JSRuntime *rt = ctx->rt;
                    free_var_ref(rt, var_ref);
                    GOTO_LABEL(exception);
                }
                pr->u.var_ref = var_ref;
                *sp++ = JS_AtomToValue(ctx, atom);
                BREAK;
            }
        HANDLER(OP_make_var_ref)
            {
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                if (JS_GetGlobalVarRef(ctx, atom, sp))
                    GOTO_LABEL(exception);
                sp += 2;
                BREAK;
            }

        HANDLER(OP_goto) {
            pc += (int32_t)get_u32(pc);
            if (unlikely(js_poll_interrupts(ctx)))
                GOTO_LABEL(exception);
            BREAK;
        }
#if SHORT_OPCODES
        HANDLER(OP_goto16) {
            pc += (int16_t)get_u16(pc);
            if (unlikely(js_poll_interrupts(ctx)))
                GOTO_LABEL(exception);
            BREAK;
        }
        HANDLER(OP_goto8) {
            pc += (int8_t)pc[0];
            if (unlikely(js_poll_interrupts(ctx)))
                GOTO_LABEL(exception);
            BREAK;
        }
#endif
        HANDLER(OP_if_true)
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 4;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (res) {
                    pc += (int32_t)get_u32(pc - 4) - 4;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_if_false)
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 4;
                /* quick and dirty test for JS_TAG_INT, JS_TAG_BOOL, JS_TAG_NULL and JS_TAG_UNDEFINED */
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (!res) {
                    pc += (int32_t)get_u32(pc - 4) - 4;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    GOTO_LABEL(exception);
                BREAK;
            }
#if SHORT_OPCODES
        HANDLER(OP_if_true8)
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 1;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (res) {
                    pc += (int8_t)pc[-1] - 1;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_if_false8)
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                pc += 1;
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1);
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp--;
                if (!res) {
                    pc += (int8_t)pc[-1] - 1;
                }
                if (unlikely(js_poll_interrupts(ctx)))
                    GOTO_LABEL(exception);
                BREAK;
            }
#endif
        HANDLER(OP_catch)
            {
                int32_t diff;
                diff = get_u32(pc);
                sp[0] = JS_NewCatchOffset(ctx, pc + diff - b->byte_code_buf);
                sp++;
                pc += 4;
                BREAK;
            }
        HANDLER(OP_gosub)
            {
                int32_t diff;
                diff = get_u32(pc);
                /* XXX: should have a different tag to avoid security flaw */
                sp[0] = JS_NewInt32(ctx, pc + 4 - b->byte_code_buf);
                sp++;
                pc += diff;
                BREAK;
            }
        HANDLER(OP_ret)
            {
                JSValue op1;
                uint32_t pos;
                op1 = sp[-1];
                if (unlikely(JS_VALUE_GET_TAG(op1) != JS_TAG_INT))
                    goto ret_fail;
                pos = JS_VALUE_GET_INT(op1);
                if (unlikely(pos >= b->byte_code_len)) {
                ret_fail:
                    JS_ThrowInternalError(ctx, "invalid ret value");
                    GOTO_LABEL(exception);
                }
                sp--;
                pc = b->byte_code_buf + pos;
                BREAK;
            }

        HANDLER(OP_for_in_start) {
            sf->cur_pc = pc;
            if (js_for_in_start(ctx, sp))
                GOTO_LABEL(exception);
            BREAK;
        }
        HANDLER(OP_for_in_next) {
            sf->cur_pc = pc;
            if (js_for_in_next(ctx, sp))
                GOTO_LABEL(exception);
            sp += 2;
            BREAK;
        }
        HANDLER(OP_for_of_start) {
            sf->cur_pc = pc;
            if (js_for_of_start(ctx, sp, FALSE))
                GOTO_LABEL(exception);
            sp += 1;
            *sp++ = JS_NewCatchOffset(ctx, 0);
            BREAK;
        }
        HANDLER(OP_for_of_next)
            {
                int offset = -3 - pc[0];
                pc += 1;
                sf->cur_pc = pc;
                if (js_for_of_next(ctx, sp, offset))
                    GOTO_LABEL(exception);
                sp += 2;
                BREAK;
            }
        HANDLER(OP_for_await_of_next) {
            sf->cur_pc = pc;
            if (js_for_await_of_next(ctx, sp))
                GOTO_LABEL(exception);
            sp++;
            BREAK;
        }
        HANDLER(OP_for_await_of_start) {
            sf->cur_pc = pc;
            if (js_for_of_start(ctx, sp, TRUE))
                GOTO_LABEL(exception);
            sp += 1;
            *sp++ = JS_NewCatchOffset(ctx, 0);
            BREAK;
        }
        HANDLER(OP_iterator_get_value_done) {
            sf->cur_pc = pc;
            if (js_iterator_get_value_done(ctx, sp))
                GOTO_LABEL(exception);
            sp += 1;
            BREAK;
        }
        HANDLER(OP_iterator_check_object) {
            if (unlikely(!JS_IsObject(sp[-1]))) {
                JS_ThrowTypeError(ctx, "iterator must return an object");
                GOTO_LABEL(exception);
            }
            BREAK;
        }

        HANDLER(OP_iterator_close) {
            /* iter_obj next catch_offset -> */
            sp--; /* drop the catch offset to avoid getting caught by exception */
            JS_FreeValue(ctx, sp[-1]); /* drop the next method */
            sp--;
            if (!JS_IsUndefined(sp[-1])) {
                sf->cur_pc = pc;
                if (JS_IteratorClose(ctx, sp[-1], FALSE))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
            }
            sp--;
            BREAK;
        }
        HANDLER(OP_nip_catch)
            {
                JSValue *stack_buf = sf->var_buf + b->var_count;
                /* catch_offset ... ret_val -> ret_eval */
                ret_val = *--sp;
                while (sp > stack_buf &&
                       JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_CATCH_OFFSET) {
                    JS_FreeValue(ctx, *--sp);
                }
                if (unlikely(sp == stack_buf)) {
                    JS_ThrowInternalError(ctx, "nip_catch");
                    JS_FreeValue(ctx, ret_val);
                    GOTO_LABEL(exception);
                }
                sp[-1] = ret_val;
                BREAK;
            }

        HANDLER(OP_iterator_next)
            /* stack: iter_obj next catch_offset val */
            {
                JSValue ret;
                sf->cur_pc = pc;
                ret = JS_Call(ctx, sp[-3], sp[-4],
                              1, (JSValueConst *)(sp - 1));
                if (JS_IsException(ret))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret;
                BREAK;
            }

        HANDLER(OP_iterator_call)
            /* stack: iter_obj next catch_offset val */
            {
                JSValue method, ret;
                BOOL ret_flag;
                int pc_flags;
                pc_flags = *pc++;
                sf->cur_pc = pc;
                method = JS_GetProperty(ctx, sp[-4], (pc_flags & 1) ?
                                        JS_ATOM_throw : JS_ATOM_return);
                if (JS_IsException(method))
                    GOTO_LABEL(exception);
                if (JS_IsUndefined(method) || JS_IsNull(method)) {
                    ret_flag = TRUE;
                } else {
                    if (pc_flags & 2) {
                        /* no argument */
                        ret = JS_CallFree(ctx, method, sp[-4],
                                          0, NULL);
                    } else {
                        ret = JS_CallFree(ctx, method, sp[-4],
                                          1, (JSValueConst *)(sp - 1));
                    }
                    if (JS_IsException(ret))
                        GOTO_LABEL(exception);
                    JS_FreeValue(ctx, sp[-1]);
                    sp[-1] = ret;
                    ret_flag = FALSE;
                }
                sp[0] = JS_NewBool(ctx, ret_flag);
                sp += 1;
                BREAK;
            }

        HANDLER(OP_lnot)
            {
                int res;
                JSValue op1;

                op1 = sp[-1];
                if ((uint32_t)JS_VALUE_GET_TAG(op1) <= JS_TAG_UNDEFINED) {
                    res = JS_VALUE_GET_INT(op1) != 0;
                } else {
                    res = JS_ToBoolFree(ctx, op1);
                }
                sp[-1] = JS_NewBool(ctx, !res);
                BREAK;
            }

        HANDLER(OP_get_field)
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                sf->cur_pc = pc;
                val = JS_GetProperty(ctx, sp[-1], atom);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = val;
                BREAK;
            }

        HANDLER(OP_get_field2)
            {
                JSValue val;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                sf->cur_pc = pc;
                val = JS_GetProperty(ctx, sp[-1], atom);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                *sp++ = val;
                BREAK;
            }

        HANDLER(OP_put_field)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                ret = JS_SetPropertyInternal(ctx, sp[-2], atom, sp[-1], sp[-2],
                                             JS_PROP_THROW_STRICT);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 2;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_private_symbol)
            {
                JSAtom atom;
                JSValue val;

                atom = get_u32(pc);
                pc += 4;
                val = JS_NewSymbolFromAtom(ctx, atom, JS_ATOM_TYPE_PRIVATE);
                if (JS_IsException(val))
                    GOTO_LABEL(exception);
                *sp++ = val;
                BREAK;
            }

        HANDLER(OP_get_private_field)
            {
                JSValue val;

                val = JS_GetPrivateField(ctx, sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp[-2] = val;
                sp--;
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_put_private_field)
            {
                int ret;
                ret = JS_SetPrivateField(ctx, sp[-3], sp[-1], sp[-2]);
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-1]);
                sp -= 3;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_define_private_field)
            {
                int ret;
                ret = JS_DefinePrivateField(ctx, sp[-3], sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 2;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_define_field)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_DefinePropertyValue(ctx, sp[-2], atom, sp[-1],
                                             JS_PROP_C_W_E | JS_PROP_THROW);
                sp--;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_set_name)
            {
                int ret;
                JSAtom atom;
                atom = get_u32(pc);
                pc += 4;

                ret = JS_DefineObjectName(ctx, sp[-1], atom, JS_PROP_CONFIGURABLE);
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_set_name_computed)
            {
                int ret;
                ret = JS_DefineObjectNameComputed(ctx, sp[-1], sp[-2], JS_PROP_CONFIGURABLE);
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }
        HANDLER(OP_set_proto)
            {
                JSValue proto;
                sf->cur_pc = pc;
                proto = sp[-1];
                if (JS_IsObject(proto) || JS_IsNull(proto)) {
                    if (JS_SetPrototypeInternal(ctx, sp[-2], proto, TRUE) < 0)
                        GOTO_LABEL(exception);
                }
                JS_FreeValue(ctx, proto);
                sp--;
                BREAK;
            }
        HANDLER(OP_set_home_object) {
            js_method_set_home_object(ctx, sp[-1], sp[-2]);
            BREAK;
        }
        HANDLER_FALLTHROUGH(OP_define_method, OP_define_method_computed)
        HANDLER(OP_define_method_computed)
            {
                int opcode_ = pc[-1];
                JSValue getter, setter, value;
                JSValueConst obj;
                JSAtom atom;
                int js_flags, ret, op_flags;
                BOOL is_computed;
#define OP_DEFINE_METHOD_METHOD 0
#define OP_DEFINE_METHOD_GETTER 1
#define OP_DEFINE_METHOD_SETTER 2
#define OP_DEFINE_METHOD_ENUMERABLE 4

                is_computed = (opcode_ == OP_define_method_computed);
                if (is_computed) {
                    atom = JS_ValueToAtom(ctx, sp[-2]);
                    if (unlikely(atom == JS_ATOM_NULL))
                        GOTO_LABEL(exception);
                    opcode_ += OP_define_method - OP_define_method_computed;
                } else {
                    atom = get_u32(pc);
                    pc += 4;
                }
                op_flags = *pc++;

                obj = sp[-2 - is_computed];
                js_flags = JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE |
                    JS_PROP_HAS_ENUMERABLE | JS_PROP_THROW;
                if (op_flags & OP_DEFINE_METHOD_ENUMERABLE)
                    js_flags |= JS_PROP_ENUMERABLE;
                op_flags &= 3;
                value = JS_UNDEFINED;
                getter = JS_UNDEFINED;
                setter = JS_UNDEFINED;
                if (op_flags == OP_DEFINE_METHOD_METHOD) {
                    value = sp[-1];
                    js_flags |= JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE | JS_PROP_WRITABLE;
                } else if (op_flags == OP_DEFINE_METHOD_GETTER) {
                    getter = sp[-1];
                    js_flags |= JS_PROP_HAS_GET;
                } else {
                    setter = sp[-1];
                    js_flags |= JS_PROP_HAS_SET;
                }
                ret = js_method_set_properties(ctx, sp[-1], atom, js_flags, obj);
                if (ret >= 0) {
                    ret = JS_DefineProperty(ctx, obj, atom, value,
                                            getter, setter, js_flags);
                }
                JS_FreeValue(ctx, sp[-1]);
                if (is_computed) {
                    JS_FreeAtom(ctx, atom);
                    JS_FreeValue(ctx, sp[-2]);
                }
                sp -= 1 + is_computed;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER_FALLTHROUGH(OP_define_class, OP_define_class_computed)
        HANDLER(OP_define_class_computed)
            {
                int opcode_ = pc[-1];
                int class_flags;
                JSAtom atom;

                atom = get_u32(pc);
                class_flags = pc[4];
                pc += 5;
                if (js_op_define_class(ctx, sp, atom, class_flags,
                                       var_refs, sf,
                                       (opcode_ == OP_define_class_computed)) < 0)
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_get_array_el)
            {
                JSValue val;

                sf->cur_pc = pc;
                val = JS_GetPropertyValue(ctx, sp[-2], sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                sp[-2] = val;
                sp--;
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_get_array_el2)
            {
                JSValue val;

                sf->cur_pc = pc;
                val = JS_GetPropertyValue(ctx, sp[-2], sp[-1]);
                sp[-1] = val;
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_get_array_el3)
            {
                JSValue val;

                switch (JS_VALUE_GET_TAG(sp[-2])) {
                case JS_TAG_INT:
                case JS_TAG_STRING:
                case JS_TAG_SYMBOL:
                    /* undefined and null are tested in JS_GetPropertyValue() */
                    break;
                default:
                    /* must be tested nefore JS_ToPropertyKey */
                    if (unlikely(JS_IsUndefined(sp[-2]) || JS_IsNull(sp[-2]))) {
                        JS_ThrowTypeError(ctx, "value has no property");
                        GOTO_LABEL(exception);
                    }
                    sf->cur_pc = pc;
                    ret_val = JS_ToPropertyKey(ctx, sp[-1]);
                    if (JS_IsException(ret_val))
                        GOTO_LABEL(exception);
                    JS_FreeValue(ctx, sp[-1]);
                    sp[-1] = ret_val;
                    break;
                }
                sf->cur_pc = pc;
                val = JS_GetPropertyValue(ctx, sp[-2], JS_DupValue(ctx, sp[-1]));
                *sp++ = val;
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                BREAK;
            }
            
        HANDLER(OP_get_ref_value)
            {
                JSValue val;
                JSAtom atom;
                int ret;
                
                sf->cur_pc = pc;
                atom = JS_ValueToAtom(ctx, sp[-1]);
                if (atom == JS_ATOM_NULL)
                    GOTO_LABEL(exception);
                if (unlikely(JS_IsUndefined(sp[-2]))) {
                    JS_ThrowReferenceErrorNotDefined(ctx, atom);
                    JS_FreeAtom(ctx, atom);
                    GOTO_LABEL(exception);
                }
                ret = JS_HasProperty(ctx, sp[-2], atom);
                if (unlikely(ret <= 0)) {
                    if (ret < 0) {
                        JS_FreeAtom(ctx, atom);
                        GOTO_LABEL(exception);
                    }
                    if (is_strict_mode(ctx)) {
                        JS_ThrowReferenceErrorNotDefined(ctx, atom);
                        JS_FreeAtom(ctx, atom);
                        GOTO_LABEL(exception);
                    } 
                    val = JS_UNDEFINED;
                } else {
                    val = JS_GetProperty(ctx, sp[-2], atom);
                }
                JS_FreeAtom(ctx, atom);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                sp[0] = val;
                sp++;
                BREAK;
            }

        HANDLER(OP_get_super_value)
            {
                JSValue val;
                JSAtom atom;
                sf->cur_pc = pc;
                atom = JS_ValueToAtom(ctx, sp[-1]);
                if (unlikely(atom == JS_ATOM_NULL))
                    GOTO_LABEL(exception);
                val = JS_GetPropertyInternal(ctx, sp[-2], atom, sp[-3], FALSE);
                JS_FreeAtom(ctx, atom);
                if (unlikely(JS_IsException(val)))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-3]);
                sp[-3] = val;
                sp -= 2;
                BREAK;
            }

        HANDLER(OP_put_array_el)
            {
                int ret;

                sf->cur_pc = pc;
                ret = JS_SetPropertyValue(ctx, sp[-3], sp[-2], sp[-1], JS_PROP_THROW_STRICT);
                JS_FreeValue(ctx, sp[-3]);
                sp -= 3;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_put_ref_value)
            {
                int ret;
                JSAtom atom;
                sf->cur_pc = pc;
                atom = JS_ValueToAtom(ctx, sp[-2]);
                if (unlikely(atom == JS_ATOM_NULL))
                    GOTO_LABEL(exception);
                if (unlikely(JS_IsUndefined(sp[-3]))) {
                    if (is_strict_mode(ctx)) {
                        JS_ThrowReferenceErrorNotDefined(ctx, atom);
                        JS_FreeAtom(ctx, atom);
                        GOTO_LABEL(exception);
                    } else {
                        sp[-3] = JS_DupValue(ctx, ctx->global_obj);
                    }
                }
                ret = JS_HasProperty(ctx, sp[-3], atom);
                if (unlikely(ret <= 0)) {
                    if (unlikely(ret < 0)) {
                        JS_FreeAtom(ctx, atom);
                        GOTO_LABEL(exception);
                    }
                    if (is_strict_mode(ctx)) {
                        JS_ThrowReferenceErrorNotDefined(ctx, atom);
                        JS_FreeAtom(ctx, atom);
                        GOTO_LABEL(exception);
                    }
                }
                ret = JS_SetPropertyInternal(ctx, sp[-3], atom, sp[-1], sp[-3], JS_PROP_THROW_STRICT);
                JS_FreeAtom(ctx, atom);
                JS_FreeValue(ctx, sp[-2]);
                JS_FreeValue(ctx, sp[-3]);
                sp -= 3;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_put_super_value)
            {
                int ret;
                JSAtom atom;
                sf->cur_pc = pc;
                if (JS_VALUE_GET_TAG(sp[-3]) != JS_TAG_OBJECT) {
                    JS_ThrowTypeErrorNotAnObject(ctx);
                    GOTO_LABEL(exception);
                }
                atom = JS_ValueToAtom(ctx, sp[-2]);
                if (unlikely(atom == JS_ATOM_NULL))
                    GOTO_LABEL(exception);
                ret = JS_SetPropertyInternal(ctx, sp[-3], atom, sp[-1], sp[-4],
                                             JS_PROP_THROW_STRICT);
                JS_FreeAtom(ctx, atom);
                JS_FreeValue(ctx, sp[-4]);
                JS_FreeValue(ctx, sp[-3]);
                JS_FreeValue(ctx, sp[-2]);
                sp -= 4;
                if (ret < 0)
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_define_array_el)
            {
                int ret;
                ret = JS_DefinePropertyValueValue(ctx, sp[-3], JS_DupValue(ctx, sp[-2]), sp[-1],
                                                  JS_PROP_C_W_E | JS_PROP_THROW);
                sp -= 1;
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_append)    /* array pos enumobj -- array pos */
            {
                sf->cur_pc = pc;
                if (js_append_enumerate(ctx, sp))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, *--sp);
                BREAK;
            }

        HANDLER(OP_copy_data_properties) {    /* target source excludeList */
                /* stack offsets (-1 based):
                   2 bits for target,
                   3 bits for source,
                   2 bits for exclusionList */
                int mask;

                mask = *pc++;
                sf->cur_pc = pc;
                if (JS_CopyDataProperties(ctx, sp[-1 - (mask & 3)],
                                          sp[-1 - ((mask >> 2) & 7)],
                                          sp[-1 - ((mask >> 5) & 7)], 0))
                    GOTO_LABEL(exception);
                BREAK;
            }

        HANDLER(OP_add)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(op1) + JS_VALUE_GET_INT(op2);
                    if (unlikely((int)r != r))
                        goto add_slow;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
                    sp[-2] = __JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(op1) +
                                             JS_VALUE_GET_FLOAT64(op2));
                    sp--;
                } else if (JS_IsString(op1) && JS_IsString(op2)) {
                    sp[-2] = JS_ConcatString(ctx, op1, op2);
                    sp--;
                    if (JS_IsException(sp[-1]))
                        GOTO_LABEL(exception);
                } else {
                add_slow:
                    sf->cur_pc = pc;
                    if (js_add_slow(ctx, sp))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_add_loc)
            {
                JSValue op2;
                JSValue *pv;
                int idx;
                idx = *pc;
                pc += 1;

                op2 = sp[-1];
                pv = &var_buf[idx];
                if (likely(JS_VALUE_IS_BOTH_INT(*pv, op2))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(*pv) + JS_VALUE_GET_INT(op2);
                    if (unlikely((int)r != r))
                        goto add_loc_slow;
                    *pv = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(*pv, op2)) {
                    *pv = __JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(*pv) +
                                               JS_VALUE_GET_FLOAT64(op2));
                    sp--;
                } else if (JS_VALUE_GET_TAG(*pv) == JS_TAG_STRING) {
                    sp--;
                    sf->cur_pc = pc;
                    op2 = JS_ToPrimitiveFree(ctx, op2, HINT_NONE);
                    if (JS_IsException(op2))
                        GOTO_LABEL(exception);
                    if (JS_ConcatStringInPlace(ctx, JS_VALUE_GET_STRING(*pv), op2)) {
                        JS_FreeValue(ctx, op2);
                    } else {
                        op2 = JS_ConcatString(ctx, JS_DupValue(ctx, *pv), op2);
                        if (JS_IsException(op2))
                            GOTO_LABEL(exception);
                        set_value(ctx, pv, op2);
                    }
                } else {
                    JSValue ops[2];
                add_loc_slow:
                    /* In case of exception, js_add_slow frees ops[0]
                       and ops[1], so we must duplicate *pv */
                    sf->cur_pc = pc;
                    ops[0] = JS_DupValue(ctx, *pv);
                    ops[1] = op2;
                    sp--;
                    if (js_add_slow(ctx, ops + 2))
                        GOTO_LABEL(exception);
                    set_value(ctx, pv, ops[0]);
                }
                BREAK;
            }
        HANDLER(OP_sub)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int64_t r;
                    r = (int64_t)JS_VALUE_GET_INT(op1) - JS_VALUE_GET_INT(op2);
                    if (unlikely((int)r != r))
                        goto binary_arith_slow_sub;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
                    sp[-2] = __JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(op1) -
                                             JS_VALUE_GET_FLOAT64(op2));
                    sp--;
                } else {
                  binary_arith_slow_sub:
                    sf->cur_pc = pc;
                    if (js_binary_arith_slow(ctx, sp, OP_sub))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_mul)
            {
                JSValue op1, op2;
                double d;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int32_t v1, v2;
                    int64_t r;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    r = (int64_t)v1 * v2;
                    if (unlikely((int)r != r)) {
                        d = (double)r;
                        goto mul_fp_res;
                    }
                    /* need to test zero case for -0 result */
                    if (unlikely(r == 0 && (v1 | v2) < 0)) {
                        d = -0.0;
                        goto mul_fp_res;
                    }
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else if (JS_VALUE_IS_BOTH_FLOAT(op1, op2)) {
                    d = JS_VALUE_GET_FLOAT64(op1) * JS_VALUE_GET_FLOAT64(op2);
                mul_fp_res:
                    sp[-2] = __JS_NewFloat64(ctx, d);
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_arith_slow(ctx, sp, OP_mul))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_div)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int v1, v2;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    sp[-2] = JS_NewFloat64(ctx, (double)v1 / (double)v2);
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_arith_slow(ctx, sp, OP_div))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_mod)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    int v1, v2, r;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    /* We must avoid v2 = 0, v1 = INT32_MIN and v2 =
                       -1 and the cases where the result is -0. */
                    if (unlikely(v1 < 0 || v2 <= 0))
                        goto binary_arith_slow_mod;
                    r = v1 % v2;
                    sp[-2] = JS_NewInt32(ctx, r);
                    sp--;
                } else {
                  binary_arith_slow_mod:
                    sf->cur_pc = pc;
                    if (js_binary_arith_slow(ctx, sp, OP_mod))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_pow) {
            sf->cur_pc = pc;
            if (js_binary_arith_slow(ctx, sp, OP_pow))
                GOTO_LABEL(exception);
            sp--;
            BREAK;
        }

        HANDLER(OP_plus)
            {
                JSValue op1;
                uint32_t tag;
                op1 = sp[-1];
                tag = JS_VALUE_GET_TAG(op1);
                if (tag == JS_TAG_INT || JS_TAG_IS_FLOAT64(tag)) {
                } else {
                    sf->cur_pc = pc;
                    if (js_unary_arith_slow(ctx, sp, OP_plus))
                        GOTO_LABEL(exception);
                }
                BREAK;
            }
        HANDLER(OP_neg)
            {
                JSValue op1;
                uint32_t tag;
                int val;
                double d;
                op1 = sp[-1];
                tag = JS_VALUE_GET_TAG(op1);
                if (tag == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    /* Note: -0 cannot be expressed as integer */
                    if (unlikely(val == 0)) {
                        d = -0.0;
                        goto neg_fp_res;
                    }
                    if (unlikely(val == INT32_MIN)) {
                        d = -(double)val;
                        goto neg_fp_res;
                    }
                    sp[-1] = JS_NewInt32(ctx, -val);
                } else if (JS_TAG_IS_FLOAT64(tag)) {
                    d = -JS_VALUE_GET_FLOAT64(op1);
                neg_fp_res:
                    sp[-1] = __JS_NewFloat64(ctx, d);
                } else {
                    sf->cur_pc = pc;
                    if (js_unary_arith_slow(ctx, sp, OP_neg))
                        GOTO_LABEL(exception);
                }
                BREAK;
            }
        HANDLER(OP_inc)
            {
                JSValue op1;
                int val;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MAX))
                        goto inc_slow;
                    sp[-1] = JS_NewInt32(ctx, val + 1);
                } else {
                inc_slow:
                    sf->cur_pc = pc;
                    if (js_unary_arith_slow(ctx, sp, OP_inc))
                        GOTO_LABEL(exception);
                }
                BREAK;
            }
        HANDLER(OP_dec)
            {
                JSValue op1;
                int val;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MIN))
                        goto dec_slow;
                    sp[-1] = JS_NewInt32(ctx, val - 1);
                } else {
                dec_slow:
                    sf->cur_pc = pc;
                    if (js_unary_arith_slow(ctx, sp, OP_dec))
                        GOTO_LABEL(exception);
                }
                BREAK;
            }
        HANDLER(OP_post_inc)
          {
            sf->cur_pc = pc;
            if (js_post_inc_slow(ctx, sp, OP_post_inc))
                GOTO_LABEL(exception);
            sp++;
            BREAK;
          }
        HANDLER(OP_post_dec)
          {
            sf->cur_pc = pc;
            if (js_post_inc_slow(ctx, sp, OP_post_dec))
                GOTO_LABEL(exception);
            sp++;
            BREAK;
          }
        HANDLER(OP_inc_loc)
            {
                JSValue op1;
                int val;
                int idx;
                idx = *pc;
                pc += 1;

                op1 = var_buf[idx];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MAX))
                        goto inc_loc_slow;
                    var_buf[idx] = JS_NewInt32(ctx, val + 1);
                } else {
                inc_loc_slow:
                    sf->cur_pc = pc;
                    /* must duplicate otherwise the variable value may
                       be destroyed before JS code accesses it */
                    op1 = JS_DupValue(ctx, op1);
                    if (js_unary_arith_slow(ctx, &op1 + 1, OP_inc))
                        GOTO_LABEL(exception);
                    set_value(ctx, &var_buf[idx], op1);
                }
                BREAK;
            }
        HANDLER(OP_dec_loc)
            {
                JSValue op1;
                int val;
                int idx;
                idx = *pc;
                pc += 1;

                op1 = var_buf[idx];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    val = JS_VALUE_GET_INT(op1);
                    if (unlikely(val == INT32_MIN))
                        goto dec_loc_slow;
                    var_buf[idx] = JS_NewInt32(ctx, val - 1);
                } else {
                dec_loc_slow:
                    sf->cur_pc = pc;
                    /* must duplicate otherwise the variable value may
                       be destroyed before JS code accesses it */
                    op1 = JS_DupValue(ctx, op1);
                    if (js_unary_arith_slow(ctx, &op1 + 1, OP_dec))
                        GOTO_LABEL(exception);
                    set_value(ctx, &var_buf[idx], op1);
                }
                BREAK;
            }
        HANDLER(OP_not)
            {
                JSValue op1;
                op1 = sp[-1];
                if (JS_VALUE_GET_TAG(op1) == JS_TAG_INT) {
                    sp[-1] = JS_NewInt32(ctx, ~JS_VALUE_GET_INT(op1));
                } else {
                    sf->cur_pc = pc;
                    if (js_not_slow(ctx, sp))
                        GOTO_LABEL(exception);
                }
                BREAK;
            }

        HANDLER(OP_shl)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v1, v2;
                    v1 = JS_VALUE_GET_INT(op1);
                    v2 = JS_VALUE_GET_INT(op2);
                    v2 &= 0x1f;
                    sp[-2] = JS_NewInt32(ctx, v1 << v2);
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_logic_slow(ctx, sp, OP_shl))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_shr)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v2;
                    v2 = JS_VALUE_GET_INT(op2);
                    v2 &= 0x1f;
                    sp[-2] = JS_NewUint32(ctx,
                                          (uint32_t)JS_VALUE_GET_INT(op1) >>
                                          v2);
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_shr_slow(ctx, sp))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_sar)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    uint32_t v2;
                    v2 = JS_VALUE_GET_INT(op2);
                    v2 &= 0x1f;
                    sp[-2] = JS_NewInt32(ctx,
                                          (int)JS_VALUE_GET_INT(op1) >> v2);
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_logic_slow(ctx, sp, OP_sar))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_and)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) &
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_logic_slow(ctx, sp, OP_and))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_or)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) |
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_logic_slow(ctx, sp, OP_or))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }
        HANDLER(OP_xor)
            {
                JSValue op1, op2;
                op1 = sp[-2];
                op2 = sp[-1];
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {
                    sp[-2] = JS_NewInt32(ctx,
                                         JS_VALUE_GET_INT(op1) ^
                                         JS_VALUE_GET_INT(op2));
                    sp--;
                } else {
                    sf->cur_pc = pc;
                    if (js_binary_logic_slow(ctx, sp, OP_xor))
                        GOTO_LABEL(exception);
                    sp--;
                }
                BREAK;
            }

#define OP_CMP(opcode, binary_op, slow_call)              \
            HANDLER(opcode) {                             \
                JSValue op1, op2;                         \
                op1 = sp[-2];                             \
                op2 = sp[-1];                                   \
                if (likely(JS_VALUE_IS_BOTH_INT(op1, op2))) {           \
                    sp[-2] = JS_NewBool(ctx, JS_VALUE_GET_INT(op1) binary_op JS_VALUE_GET_INT(op2)); \
                    sp--;                                               \
                } else {                                                \
                    sf->cur_pc = pc;                                    \
                    if (slow_call)                                      \
                        GOTO_LABEL(exception);                                 \
                    sp--;                                               \
                }                                                       \
                BREAK;                                                  \
            }

            OP_CMP(OP_lt, <, js_relational_slow(ctx, sp, OP_lt));
            OP_CMP(OP_lte, <=, js_relational_slow(ctx, sp, OP_lte));
            OP_CMP(OP_gt, >, js_relational_slow(ctx, sp, OP_gt));
            OP_CMP(OP_gte, >=, js_relational_slow(ctx, sp, OP_gte));
            OP_CMP(OP_eq, ==, js_eq_slow(ctx, sp, 0));
            OP_CMP(OP_neq, !=, js_eq_slow(ctx, sp, 1));
            OP_CMP(OP_strict_eq, ==, js_strict_eq_slow(ctx, sp, 0));
            OP_CMP(OP_strict_neq, !=, js_strict_eq_slow(ctx, sp, 1));

        HANDLER(OP_in) {
            sf->cur_pc = pc;
            if (js_operator_in(ctx, sp))
                GOTO_LABEL(exception);
            sp--;
            BREAK;
        }
        HANDLER(OP_private_in) {
            sf->cur_pc = pc;
            if (js_operator_private_in(ctx, sp))
                GOTO_LABEL(exception);
            sp--;
            BREAK;
        }
        HANDLER(OP_instanceof) {
            sf->cur_pc = pc;
            if (js_operator_instanceof(ctx, sp))
                GOTO_LABEL(exception);
            sp--;
            BREAK;
        }
        HANDLER(OP_typeof)
            {
                JSValue op1;
                JSAtom atom;

                op1 = sp[-1];
                atom = js_operator_typeof(ctx, op1);
                JS_FreeValue(ctx, op1);
                sp[-1] = JS_AtomToString(ctx, atom);
                BREAK;
            }
        HANDLER(OP_delete) {
            sf->cur_pc = pc;
            if (js_operator_delete(ctx, sp))
                GOTO_LABEL(exception);
            sp--;
            BREAK;
        }
        HANDLER(OP_delete_var)
            {
                JSAtom atom;
                int ret;

                atom = get_u32(pc);
                pc += 4;
                sf->cur_pc = pc;

                ret = JS_DeleteGlobalVar(ctx, atom);
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                *sp++ = JS_NewBool(ctx, ret);
                BREAK;
            }

        HANDLER(OP_to_object) {
            if (JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_OBJECT) {
                sf->cur_pc = pc;
                ret_val = JS_ToObject(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
            }
            BREAK;
        }

        HANDLER(OP_to_propkey) {
            switch (JS_VALUE_GET_TAG(sp[-1])) {
            case JS_TAG_INT:
            case JS_TAG_STRING:
            case JS_TAG_SYMBOL:
                break;
            default:
                sf->cur_pc = pc;
                ret_val = JS_ToPropertyKey(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
                break;
            }
            BREAK;
        }

#if 0
        HANDLER(OP_to_string)
            if (JS_VALUE_GET_TAG(sp[-1]) != JS_TAG_STRING) {
                ret_val = JS_ToString(ctx, sp[-1]);
                if (JS_IsException(ret_val))
                    GOTO_LABEL(exception);
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = ret_val;
            }
            BREAK;
#endif
        HANDLER_FALLTHROUGH(OP_with_get_var, OP_with_get_ref)
        HANDLER_FALLTHROUGH(OP_with_put_var, OP_with_get_ref)
        HANDLER_FALLTHROUGH(OP_with_delete_var, OP_with_get_ref)
        HANDLER_FALLTHROUGH(OP_with_make_ref, OP_with_get_ref)
        HANDLER(OP_with_get_ref)
            {
                int opcode_ = pc[-1];
                JSAtom atom;
                int32_t diff;
                JSValue obj, val;
                int ret, is_with;
                atom = get_u32(pc);
                diff = get_u32(pc + 4);
                is_with = pc[8];
                pc += 9;
                sf->cur_pc = pc;

                obj = sp[-1];
                ret = JS_HasProperty(ctx, obj, atom);
                if (unlikely(ret < 0))
                    GOTO_LABEL(exception);
                if (ret) {
                    if (is_with) {
                        ret = js_has_unscopable(ctx, obj, atom);
                        if (unlikely(ret < 0))
                            GOTO_LABEL(exception);
                        if (ret)
                            goto no_with;
                    }
                    switch (opcode_) {
                    case OP_with_get_var:
                        /* in Object Environment Records, GetBindingValue() calls HasProperty() */
                        ret = JS_HasProperty(ctx, obj, atom);
                        if (unlikely(ret <= 0)) {
                            if (ret < 0)
                                GOTO_LABEL(exception);
                            if (is_strict_mode(ctx)) {
                                JS_ThrowReferenceErrorNotDefined(ctx, atom);
                                GOTO_LABEL(exception);
                            } 
                            val = JS_UNDEFINED;
                        } else {
                            val = JS_GetProperty(ctx, obj, atom);
                            if (unlikely(JS_IsException(val)))
                                GOTO_LABEL(exception);
                        }
                        set_value(ctx, &sp[-1], val);
                        break;
                    case OP_with_put_var: /* used e.g. in for in/of */
                        /* in Object Environment Records, SetMutableBinding() calls HasProperty() */
                        ret = JS_HasProperty(ctx, obj, atom);
                        if (unlikely(ret <= 0)) {
                            if (ret < 0)
                                GOTO_LABEL(exception);
                            if (is_strict_mode(ctx)) {
                                JS_ThrowReferenceErrorNotDefined(ctx, atom);
                                GOTO_LABEL(exception);
                            } 
                        }
                        ret = JS_SetPropertyInternal(ctx, obj, atom, sp[-2], obj,
                                                     JS_PROP_THROW_STRICT);
                        JS_FreeValue(ctx, sp[-1]);
                        sp -= 2;
                        if (unlikely(ret < 0))
                            GOTO_LABEL(exception);
                        break;
                    case OP_with_delete_var:
                        ret = JS_DeleteProperty(ctx, obj, atom, 0);
                        if (unlikely(ret < 0))
                            GOTO_LABEL(exception);
                        JS_FreeValue(ctx, sp[-1]);
                        sp[-1] = JS_NewBool(ctx, ret);
                        break;
                    case OP_with_make_ref:
                        /* produce a pair object/propname on the stack */
                        *sp++ = JS_AtomToValue(ctx, atom);
                        break;
                    case OP_with_get_ref:
                        /* produce a pair object/method on the stack */
                        /* in Object Environment Records, GetBindingValue() calls HasProperty() */
                        ret = JS_HasProperty(ctx, obj, atom);
                        if (unlikely(ret < 0))
                            GOTO_LABEL(exception);
                        if (!ret) {
                            val = JS_UNDEFINED;
                        } else {
                            val = JS_GetProperty(ctx, obj, atom);
                            if (unlikely(JS_IsException(val)))
                                GOTO_LABEL(exception);
                        }
                        *sp++ = val;
                        break;
                    }
                    pc += diff - 5;
                } else {
                no_with:
                    /* if not jumping, drop the object argument */
                    JS_FreeValue(ctx, sp[-1]);
                    sp--;
                }
                BREAK;
            }

        HANDLER(OP_await) {
            ret_val = JS_NewInt32(ctx, FUNC_RET_AWAIT);
            GOTO_LABEL(done_generator);
        }
        HANDLER(OP_yield) {
            ret_val = JS_NewInt32(ctx, FUNC_RET_YIELD);
            GOTO_LABEL(done_generator);
        }
        HANDLER_FALLTHROUGH(OP_yield_star, OP_async_yield_star)
        HANDLER(OP_async_yield_star) {
            ret_val = JS_NewInt32(ctx, FUNC_RET_YIELD_STAR);
            GOTO_LABEL(done_generator);
        }
        HANDLER(OP_return_async) {
            ret_val = JS_UNDEFINED;
            GOTO_LABEL(done_generator);
        }
        HANDLER(OP_initial_yield) {
            ret_val = JS_NewInt32(ctx, FUNC_RET_INITIAL_YIELD);
            GOTO_LABEL(done_generator);
        }

        HANDLER(OP_nop) {
            BREAK;
        }
        HANDLER(OP_is_undefined_or_null) {
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_UNDEFINED ||
                JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_NULL) {
                sp[-1] = JS_TRUE;
            } else {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_FALSE;
            }
            BREAK;
        }
#if SHORT_OPCODES
        HANDLER(OP_is_undefined) {
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_UNDEFINED) {
                sp[-1] = JS_TRUE;
            } else {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_FALSE;
            }
            BREAK;
        }
        HANDLER(OP_is_null) {
            if (JS_VALUE_GET_TAG(sp[-1]) == JS_TAG_NULL) {
                sp[-1] = JS_TRUE;
            } else {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_FALSE;
            }
            /* XXX: could merge to a single opcode */
            BREAK;
        }
        HANDLER(OP_typeof_is_undefined) {
            /* different from OP_is_undefined because of isHTMLDDA */
            if (js_operator_typeof(ctx, sp[-1]) == JS_ATOM_undefined) {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_TRUE;
            } else {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_FALSE;
            }
            BREAK;
        }
        HANDLER(OP_typeof_is_function) {
            if (js_operator_typeof(ctx, sp[-1]) == JS_ATOM_function) {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_TRUE;
            } else {
                JS_FreeValue(ctx, sp[-1]);
                sp[-1] = JS_FALSE;
            }
            BREAK;
        }
#endif
        HANDLER(OP_invalid)
        DEFAULT {
            int opcode = pc[-1];
            JS_ThrowInternalError(ctx, "invalid opcode: pc=%u opcode=0x%02x",
                                  (int)(pc - b->byte_code_buf - 1), opcode);
            GOTO_LABEL(exception);
        }

#if !TAIL_DISPATCH
        }  /* SWITCH(pc) */
    }  /* for(;;) */
    GOTO_LABEL(exception);
#endif

/*****************************************************************************/

#if TAIL_DISPATCH
LABEL_HANDLER(exception) {
    JSRuntime *rt = caller_ctx->rt;
    JSValue *stack_buf = sf->var_buf + b->var_count;
#else
exception:
#endif
    if (is_backtrace_needed(ctx, rt->current_exception)) {
        /* add the backtrace information now (it is not done
           before if the exception happens in a bytecode
           operation */
        sf->cur_pc = pc;
        build_backtrace(ctx, rt->current_exception, NULL, 0, 0, 0);
    }
    if (!rt->current_exception_is_uncatchable) {
        while (sp > stack_buf) {
            JSValue val = *--sp;
            JS_FreeValue(ctx, val);
            if (JS_VALUE_GET_TAG(val) == JS_TAG_CATCH_OFFSET) {
                int pos = JS_VALUE_GET_INT(val);
                if (pos == 0) {
                    /* enumerator: close it with a throw */
                    JS_FreeValue(ctx, sp[-1]); /* drop the next method */
                    sp--;
                    JS_IteratorClose(ctx, sp[-1], TRUE);
                } else {
                    *sp++ = rt->current_exception;
                    rt->current_exception = JS_UNINITIALIZED;
                    pc = b->byte_code_buf + pos;
                    goto restart;
                }
            }
        }
    }
    ret_val = JS_EXCEPTION;
    /* the local variables are freed by the caller in the generator
       case. Hence the label 'done' should never be reached in a
       generator function. */
    if (b->func_kind != JS_FUNC_NORMAL)
        GOTO_LABEL(done_generator);
    GOTO_LABEL(done);
#if TAIL_DISPATCH
  restart: return jsci_jump_table[*pc](TAIL_CALL_ARGS(pc+1));
}
#endif

#if TAIL_DISPATCH
LABEL_HANDLER(done) {
    JSRuntime *rt = caller_ctx->rt;
    JSValue *pval;
#else
done:
#endif
    if (unlikely(!list_empty(&sf->var_ref_list))) {
        /* variable references reference the stack: must close them */
        close_var_refs(rt, sf);
    }
    /* free the local variables and stack */
    for(pval = local_buf; pval < sp; pval++) {
        JS_FreeValue(ctx, *pval);
    }
    caller_ctx->rt->current_stack_frame = sf->prev_frame;
    return ret_val;
#if TAIL_DISPATCH
}
#endif

#if TAIL_DISPATCH
LABEL_HANDLER(done_generator) {
    JSRuntime *rt = caller_ctx->rt;
#else
done_generator:
#endif
    sf->cur_pc = pc;
    sf->cur_sp = sp;
    rt->current_stack_frame = sf->prev_frame;
    return ret_val;
#if TAIL_DISPATCH
}
#endif

#if !TAIL_DISPATCH
}  /* end of JS_CallInternal() */
#endif

#undef HANDLER
