set -x
set -e
if [ ! -e ~/otp-19.1/bin/erl ]; then
  curl -O http://erlang.shiguredo.jp/otp_src_19.1.tar.gz
  tar xzf otp_src_19.1.tar.gz
  cd otp_src_19.1
  ./configure --prefix=/home/ubuntu/otp-19.1 \
              --enable-smp-support \
              --enable-m64-build \
              --enable-threads \
              --enable-kernel-poll \
              --enable-hipe \
              --enable-dirty-schedulers \
              --enable-sharing-preserving \
              --disable-native-libs \
              --disable-sctp \
              --without-javac
  make;
  make install;
fi
