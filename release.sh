#!/bin/bash -e

OTP_RAW_VER=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell)
OTP_VER=$(echo "${OTP_RAW_VER}" | sed -r 's/\"([0-9]+)\"/\1/g' | sed 's/[^0-9]*//g')
EXPECTED_OTP=("24" "25" "26" "27")

yes_or_exit() {
    while true; do
        read -r -p "$1 " yn
        case $yn in
            [Yy][Ee][sS]) true; break ;;
            [Nn][Oo]) echo "Ok, bye bye!"; exit ;;
            * ) echo "Please answer 'yes' or 'no'." ;;
        esac
    done
}

if [ "${OTP_VER}" -lt "24" ]; then
     echo "You are using Erlang/OTP ${OTP_VER}."
     echo "However, ex_doc must be built using OTP >= 24 for compatibility. Giving up..."
     exit
fi

VERSION=$1

if [ "${VERSION}" == "" ]; then
    echo "Error: a version argument corresponding to a tag must be supplied!"
    exit
fi

read -r -p "Do you want to recreate _checkouts/rebar3_ex_doc? [yes/no] " yn
if [ $yn == "yes" ]; then
    echo "Deleting _checkouts/rebar3_ex_doc..."
    rm -rf _checkouts/rebar3_ex_doc

    mkdir -p  _checkouts

    cd _checkouts

    git clone https://github.com/starbelly/rebar3_ex_doc

    cd rebar3_ex_doc
    mkdir priv
else
    cd _checkouts/rebar3_ex_doc
fi

git checkout "v${VERSION}"

EX_DOC_VER="v$(grep -m1 "@ex_doc_version" mix.exs | awk '{print $2}' | tr -d '"')"
echo "Fetching ex_doc ${EX_DOC_VER} escript from GitHub ..."
curl -L -o ex_doc_otp_${OTP_VER} "https://github.com/elixir-lang/ex_doc/releases/download/${EX_DOC_VER}/ex_doc_otp_${OTP_VER}"
curl -L -o ex-doc-otp-${OTP_VER}.sha256sum  "https://github.com/elixir-lang/ex_doc/releases/download/${EX_DOC_VER}/ex-doc-otp-${OTP_VER}.sha256sum"

echo "Validating integrity of ex_doc download..."

sha256sum -c ex-doc-otp-${OTP_VER}.sha256sum

chmod +x ex_doc_otp_${OTP_VER}
mv ex_doc_otp_${OTP_VER} priv
mv  ex-doc-otp-${OTP_VER}.sha256sum priv

echo "Looks good!"

SIZE=$(du -ah priv/ex_doc_otp_${OTP_VER} | awk '{print $1}')
echo "Size of priv/ex_doc_otp_${OTP_VER}: $SIZE"

rebar3 ex_doc

echo
echo "Documentation has been generated for rebar3_ex_doc using rebar3_ex_doc."
echo "This should be checked before publishing."
echo

yes_or_exit "Open up the documentation?"

open doc/index.html

echo
yes_or_exit  "Did everything look ok?"

for OTP in "${EXPECTED_OTP[@]}"
do
    if [ ! -f "priv/ex_doc_otp_${OTP}" ]; then
        echo "Expected file 'priv/ex_doc_otp_${OTP}' does not exist."
        exit 1
    fi
done

rebar3 hex publish
