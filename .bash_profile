# ## PATH setup: see manpage for path_helper

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

echo "PATH at .bash_profile startup:" $PATH
path

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/local/lib/pkgconfig
export GNUTERM="aqua"

ANT_HOME=/usr/local/java/ant
# Jetty Config
export JETTY_HOME=/usr/local/java/jetty
# export JETTY_USER=_jetty
export JETTY_LOGS=/var/log/jetty
export CLOJURESCRIPT_HOME=~/src/clojurescript
export CLJ_JAR=~/.m2/repository/org/clojure/clojure/1.6.0/clojure-1.6.0.jar
export DITAOT_HOME=/usr/local/ditaot
export DITAC_HOME=/usr/local/ditac
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

JAVA_VERSION=1.7
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

export PATH=~/.cabal/bin:$PATH

# android
export ANDROID_HOME="/Applications/Android Studio.app/sdk"

export PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH

export JAVA_HOME=`/usr/libexec/java_home -v $JAVA_VERSION`

export CLASSPATH=\"".:$JAVA_HOME/lib/tools.jar:/Applications/netrexx/lib/NetRexxC.jar:src/main/nrx:build/classes/debug:"$ANDROID_HOME"/platforms/android-19/android.jar:$ANDROID_HOME/extras/android/support/v13/android-support-v13.jar:$ANDROID_HOME/extras/android/support/v7/appcompat/libs/android-support-v7-appcompat.jar:$ANDROID_HOME/extras/android/support/v7/appcompat/libs/android-support-v4.jar:$CLASSPATH"\"

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

# Pebble SDK
export PATH=/Users/gar/pebble-dev/PebbleSDK-current/bin:$PATH
