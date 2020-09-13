# ## PATH setup: see manpage for path_helper
# echo "bash_profile"

# According to
# http://serverfault.com/questions/16355/how-to-set-global-path-on-os-x,
# only the launchd.conf method works for both CLI and GUI apps

# System:
#     0.  /etc/launchd.conf
# Bash login shell startup (man bash):
#     0.  system builds partial PATH from /etc/paths content
#         (/usr/bin:/bin:/usr/sbin:/sbin)
#     1.  /etc/profile
#         evals /usr/libexec/path_helper (see the manpage) which reads
#         a.  /etc/paths.d
#     2.  in order:
#         a.  ~/.bash_profile
#         b.  ~/.bash_login
#         c.  ~/.profile
# Bash non-login:
#     1. ~/.bashrc only
# Bash invoked as sh:
#     1. in order:  /etc/profile  ~/.profile

# ditto for MANPATH

function path(){
    old=$IFS
    IFS=:
    printf "%s\n" $PATH
    IFS=$old
}

# echo "PATH at .bash_profile startup:" $PATH
# path

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
export GNUTERM="aqua"

# Xcode
function xcode() {
if pkgutil --pkgs=com.apple.pkg.Xcode >/dev/null; then
    echo Xcode: $(pkgutil --pkg-info=com.apple.pkg.Xcode | awk '/version:/ {print $2}')
else
    echo Xcode: not installed
fi

# Command Line Tools for Xcode
if pkgutil --pkgs=com.apple.pkg.CLTools_Executables >/dev/null; then
    echo CommandLineTools: $(pkgutil --pkg-info=com.apple.pkg.CLTools_Executables | awk '/version:/ {print $2}')
else
    echo CommandLineTools: not installed
fi
}

ANT_HOME=/usr/local/java/ant
# Jetty Config
export JETTY_HOME=/usr/local/share/jetty-distribution-9.2.5.v20141112
# export JETTY_USER=_jetty
export JETTY_LOGS=/var/log/jetty
export CLOJURESCRIPT_HOME=~/src/clojurescript
export CLJ_JAR=~/.m2/repository/org/clojure/clojure/1.6.0/clojure-1.6.0.jar
export MAVEN_REPO=~/.m2/repository
export XSLT_JAR=/usr/local/jar/saxon9he.jar
export XSLUTIL_BIN=/usr/local/share/xslutil_perso-4_4_1/bin
export XML_CATALOG_FILES=/etc/xml/catalog  # for xmllint

# Fedora repository
#export FEDORA_HOME=/Application/fedora32
# export JAVA_OPTS="-Djavax.net.ssl.trustStore=$FEDORA_HOME/server/truststore  -Djavax.net.ssl.trustStorePassword=tomcat"

## Java
# export JAVA_HOME=`/usr/libexec/java_home`
# export CLASSPATH=/usr/local/jar/*:/usr/local/jar/jetty-runner.jar:/etc/xml/

function setjdk() {
  if [ $# -ne 0 ]; then
   removeFromPath '/System/Library/Frameworks/JavaVM.framework/Home/bin'
   if [ -n "${JAVA_HOME+x}" ]; then
    removeFromPath $JAVA_HOME
   fi
   export JAVA_HOME=`/usr/libexec/java_home -v $@`
   export PATH=$PATH:$JAVA_HOME/bin
  fi
 }

function removeFromPath() {
  export PATH=$(echo $PATH | sed -E -e "s;:$1;;" -e "s;$1:?;;")
}

JAVA_VERSION=11
setjdk $JAVA_VERSION


# export CLASSPATH=".:/usr/local/jar/antlr-4.2-complete.jar:$CLASSPATH"

#export JRE_HOME=

## Tomcat/Catalina
# export CATALINA_HOME=/Application/fedora32/tomcat/

## Jena et al.
# export TDBROOT=~/java/TDB-0.8.1
# export PATH=$TDBROOT/bin:$PATH

# export JOSEKIROOT=~/java/Joseki-3.3.0/


## Tomcat/Catalina
## export CATALINA_HOME=/Application/fedora32/tomcat/

## Postgresql
#export PGPORT=5432

##
# Your previous /Users/gar/.bash_profile file was backed up as /Users/gar/.bash_profile.macports-saved_2009-06-13_at_20:54:57
##

# MacPorts Installer addition on 2009-06-13_at_20:54:57: adding an appropriate PATH variable for use with MacPorts.
# export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# dump MacPorts in favor of Homebrew
# this is done in /etc/launchd.conf
# export PATH=/usr/local/bin:$PATH
#/usr/local/sbin:

# MacPorts Installer addition on 2009-06-13_at_20:54:57: adding an appropriate MANPATH variable for use with MacPorts.
# export MANPATH=/opt/local/share/man:$MANPATH
# Finished adapting your MANPATH environment variable for use with MacPorts.

# export COMA_HOME=~/work/coma
# export PATH=${COMA_HOME}/bin:$PATH

# Setting PATH for Python 3.1
# The orginal version is saved in .bash_profile.pysave
#PATH="/Library/Frameworks/Python.framework/Versions/3.1/bin:${PATH}"
#export PATH

# Setting PATH for MacPython 2.6
# The orginal version is saved in .bash_profile.pysave
#PATH="/Library/Frameworks/Python.framework/Versions/2.6/bin:${PATH}"
# PATH="${PATH}:/Applications/Octave.app/Contents/Resources/bin"
# export PATH

##
# Your previous /Users/gar/.bash_profile file was backed up as /Users/gar/.bash_profile.macports-saved_2010-09-07_at_19:38:10
##

# MacPorts Installer addition on 2010-09-07_at_19:38:10: adding an appropriate PATH variable for use with MacPorts.
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

# cappuccino
# export PATH=$PATH:/usr/local/narwhal/bin
export CAPP_BUILD="/usr/local/share/cappuccino"
export NARWHAL_ENGINE=jsc

# # gnuplot - see /etc/paths.d
# export PATH="/Applications/Gnuplot.app/Contents/Resources/bin:$PATH"

# leiningen dev
# export PATH="~/.lein/bin:$PATH"

# # git
# export PATH="/usr/local/git/bin:$PATH"
export GIT_EDITOR=vim

# clojurescript
export PATH=$PATH:$CLOJURESCRIPT_HOME/bin

# julia  -  see /etc/paths.d
# export PATH=$PATH:/Applications/Julia/Contents/Resources/julia/bin

# ant
# export PATH=$PATH:$ANT_HOME/bin

# cabal, haskell, etc.
# export PATH=$HOME/Library/Haskell/bin:$PATH


# android
export ANDROID_SDK_ROOT=~/adt-bundle-mac-x86_64-20140702/sdk
export ANDROID_NDK_HOME=$ANDROID_SDK_ROOT/ndk-bundle
export PATH=$ANDROID_SDK_ROOT/emulator:$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/tools/bin:$ANDROID_SDK_ROOT/platform-tools:$PATH

# java
export JAVA_HOME=`/usr/libexec/java_home -v $JAVA_VERSION`

export CLASSPATH=\"".:$JAVA_HOME/lib/tools.jar:/Applications/netrexx/lib/NetRexxC.jar:src/main/nrx:build/classes/debug:"$ANDROID_SDK_ROOT"/platforms/android-19/android.jar:$ANDROID_SDK_ROOT/extras/android/support/v13/android-support-v13.jar:$ANDROID_SDK_ROOT/extras/android/support/v7/appcompat/libs/android-support-v7-appcompat.jar:$ANDROID_SDK_ROOT/extras/android/support/v7/appcompat/libs/android-support-v4.jar:$CLASSPATH"\"

# Set architecture flags
export ARCHFLAGS="-arch x86_64"
# Ensure user-installed binaries take precedence
export PATH=~/bin:$PATH

# Load .bashrc if it exists
if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi

# Ruby enVironment Manager
##[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# dita-ot
export DITAOT_HOME=/usr/local/share/dita
export DITAC_HOME=/usr/local/share/ditac
export PATH=$PATH:/usr/local/share/dita/bin

GPG_TTY=$(tty)

# bazel
source ~/.bazel/bin/bazel-complete.bash

# crosstool-NG
CROSSTOOL_NG_HOME=/Volumes/CrosstoolNG

# fop
export PATH=$PATH:/usr/local/share/fop/fop

# dart stuff
export PATH=$PATH:/${HOME}/.pub-cache/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/gar/sdk/google-cloud-sdk/path.bash.inc' ]; then . '/Users/gar/sdk/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/gar/sdk/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/gar/sdk/google-cloud-sdk/completion.bash.inc'; fi

# export PATH=${HOME}:${PATH}

# graal R
# export R_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.0.2/Contents/Home/languages/R
# export PATH=${R_HOME}/bin:$PATH

export PATH=$PATH:/Applications/CoqIDE_8.11.0.app/Contents/Resources/bin

# chromium dev
# export PATH="$PATH:$HOME/sdk/depot_tools"

source $HOME/.dartrix.d/bashrc

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/gar/.sdkman"
[[ -s "/Users/gar/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/gar/.sdkman/bin/sdkman-init.sh"

# opam configuration
test -r /Users/gar/.opam/opam-init/init.sh && . /Users/gar/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export PATH="$HOME/.cargo/bin:$PATH"

export OPAMROOT="$HOME/.opam"
