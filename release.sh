#!/bin/bash -e

OTP_RAW_VER=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell)
OTP_VER=$(echo "${OTP_RAW_VER}" | sed -r 's/\"([0-9]+)\"/\1/g' | sed 's/[^0-9]*//g')

yes_or_exit() {
    while true; do
        read -r -p "$1 " yn
        case $yn in
            [Yy][Ee][sS]) true; break ;;
            [Nn][Oo]) echo "Ok, bye bye"; exit ;;
            * ) echo "Please answer yes or no." ;;
        esac
    done
}

if [ "${OTP_VER}" != "24" ]; then
     echo "You are using Erlang/OTP ${OTP_VER}"
     echo "However, ex_doc must be built using OTP == 24 for compatability. Giving up..."
     exit
fi

VERSION=$1

if [ "${VERSION}" == "" ]; then
    echo "Error: A version argument corresponding to a tag must be supplied"
    exit
fi


mix deps.get
mix escript.build
rm -f priv/ex_doc

mkdir -p  _checkouts

rm -rf _checkouts/rebar3_ex_doc

cd _checkouts

git clone https://github.com/starbelly/rebar3_ex_doc

cd rebar3_ex_doc

git checkout "v${VERSION}"

mix deps.get
mix escript.build

SIZE=$(du -ah priv/ex_doc | awk '{print $1}')
echo "Size of priv/ex_doc : $SIZE"

rebar3 ex_doc

echo
echo "Documentation has been generated for rebar3_ex_doc using rebar3_ex_doc."
echo "This should be checked before publishing"
echo

yes_or_exit "Open up the documentation?"

open doc/index.html

echo
yes_or_exit  "Did everything look ok?"

rebar3 hex publish
