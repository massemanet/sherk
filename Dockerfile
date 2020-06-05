FROM ubuntu:18.04

RUN \
        echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN \
        apt update \
        && apt install -y \
        curl sudo gnupg pkg-config libglade2-dev git emacs-nox tree

RUN \
        curl https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb > /tmp/erlang-solutions_1.0_all.deb \
        && dpkg -i /tmp/erlang-solutions_1.0_all.deb \
        && apt-get update \
        && sudo apt-get install -y \
        erlang-common-test erlang-eunit erlang-dialyzer erlang-mode erlang-parsetools erlang-dev \
        && curl https://s3.amazonaws.com/rebar3/rebar3 > /tmp/rebar3 \
        && sudo mv /tmp/rebar3 /usr/bin/rebar3 \
        && sudo chmod +x /usr/bin/rebar3

RUN \
        git clone https://github.com/massemanet/sherk \
        && cd sherk \
        && git checkout 2020 \
        && rebar3 compile
