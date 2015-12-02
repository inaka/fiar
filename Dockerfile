FROM registry.inakalabs.com/erlang:17.5

RUN apt-get update && apt-get install -y libsqlite3-dev vim
RUN mkdir /myapp
WORKDIR /myapp
COPY . /myapp
RUN mkdir -p /myapp/dump
RUN mkdir -p /root/.ssh
COPY  id_rsa /root/.ssh/id_rsa
COPY  build/known_hosts /root/.ssh/known_hosts

RUN make
COPY build/sys.config _rel/fiar/releases/0.1.0/sys.config
COPY build/fiar.monit.conf /etc/monit/conf.d/fiar.conf
