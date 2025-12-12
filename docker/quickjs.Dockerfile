# Sample standalone build command:
# podman build -t jsz-debian -f base-debian.Dockerfile . &&
# podman build -t jsz-gcc16 -f base-gcc.Dockerfile --build-arg=VER=16 . &&
# podman build -t qjs-gcc16-tail -f quickjs.Dockerfile --build-arg BASE=jsz-gcc16 --build-arg CFLAGS=-DTAIL_CALL_DISPATCH=1 --build-arg REV=tail .

ARG BASE=jsz-gcc
FROM $BASE

WORKDIR /src

ARG REPO=https://github.com/ivankra/quickjs.git
RUN git clone "$REPO" .

ARG REV=master
RUN git pull && git checkout "$REV"

ARG CFLAGS=
ARG OPT=-O2
RUN git rev-parse --short=8 HEAD >VERSION && \
    echo '%.S: %.c' >>Makefile && \
    echo '	$(CC) $(CFLAGS_OPT) -S -fverbose-asm -g -c -o $@ $<' >>Makefile && \
    sed -i "s/ -O2/ $OPT/" Makefile && \
    CFLAGS="$CFLAGS" \
    CONFIG_CLANG="$(bash -c '[[ "$CC" == *clang* ]] && echo y')" \
    make -j$(nproc) qjs quickjs.S
