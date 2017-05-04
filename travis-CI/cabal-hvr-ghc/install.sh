#!/bin/sh

set -e

. ./travis-CI/sh-lib
. ./travis-CI/custom-cabal
. ./travis-CI/dirs.list

set -x

cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

gen_custom_cabal_config
custom_retry cabal update -v
sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config
cat ${HOME}/.cabal/config

install_package() {
    cabal install -v2 $CABAL_JOBS --only-dependencies --enable-tests --enable-benchmarks
}

# install_package() {
#     id="$1"

#     if [ x"$id" = x ]; then
#         install_plan=installplan.txt
#     else
#         install_plan=installplan-${id}.txt
#     fi

#     cabal install --only-dependencies --enable-tests --enable-benchmarks --dry -v > ${install_plan}
#     sed -i -e '1,/^Resolving /d' ${install_plan}; cat ${install_plan}

#     cabsnap_dir=$HOME/.cabsnap/s-${CABALVER}

#     # check whether current requested install-plan matches cached package-db snapshot
#     if [ x"NO_CABAL_CACHE" = x ] && diff -u ${cabsnap_dir}/${install_plan} ${install_plan};
#     then
#         echo "cabal build-cache HIT";
#         rm -rfv .ghc;
#         cp -a ${cabsnap_dir}/ghc $HOME/.ghc;
#         cp -a ${cabsnap_dir}/lib ${cabsnap_dir}/share ${cabsnap_dir}/bin $HOME/.cabal/;
#     else
#         echo "cabal build-cache MISS";
#         rm -rf ${cabsnap_dir};
#         mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
#         cabal install $CABAL_JOBS --only-dependencies --enable-tests --enable-benchmarks;
#     fi

#     # snapshot package-db on cache miss
#     if [ ! -d ${cabsnap_dir} ];
#     then
#         echo "snapshotting package-db to build-cache";
#         mkdir -p ${cabsnap_dir};
#         cp -a $HOME/.ghc ${cabsnap_dir}/ghc;
#         cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin ${install_plan} ${cabsnap_dir}/;
#     fi
# }

if [ x"$dirs" = x ]; then
    install_package ''
else
    for d in $dirs; do
        ( cd $d && install_package $( echo $d | sed 's@/@_@g' ) )
    done
fi
