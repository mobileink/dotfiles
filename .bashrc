
echo "bashrc"

################################################################
##  command prompt

export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
#
# ANSI Escape Color
#
DULL=0
BRIGHT=1
#
FG_BLACK=30
FG_RED=31
FG_GREEN=32
FG_YELLOW=33
FG_BLUE=34
FG_VIOLET=35
FG_TURQUOISE=36
FG_WHITE=37
#
BG_BLACK=40
BG_RED=41
BG_GREEN=42
BG_YELLOW=43
BG_BLUE=44
BG_VIOLET=45
BG_TURQUISE=46
BG_WHITE=47

#
# ANSI Escape Commands. Change the colors here
#
ESC="\033"
NORM_MODE="$ESC[m"
USER_MODE="$ESC[${DULL};${FG_TURQUOISE};${BG_BLACK}m"
HOST_MODE="$ESC[${BRIGHT};${FG_TURQUOISE};${BG_BLACK}m"
TIME_MODE="$ESC[${DULL};${FG_GREEN};${BG_BLACK}m"
DATE_MODE="$ESC[${DULL};${FG_GREEN};${BG_BLACK}m"
DIR_MODE="$ESC[${DULL};${FG_WHITE};${BG_BLACK}m"
CPU_MODE="$ESC[${DULL};${FG_VIOLET};${BG_BLACK}m"
JOB_MODE="$ESC[${BRIGHT};${FG_RED};${BG_BLACK}m"
ERR_MODE="$ESC[${BRIGHT};${FG_RED};${BG_BLACK}m"
STRUCT_MODE="$ESC[${DULL};${FG_RED};${BG_BLACK}m"

################################################################
##    COMMANDS
function path(){
    old=$IFS
    IFS=:
    printf "%s\n" $PATH
    IFS=$old
}
#prompt_command() function
# Has commands to run before each prompt display.
# Don't put a lot of characters in PS1, because then bash gets confused about
# the length of the prompt and starts word-wrapping before it should. Unfortunately,
# instead of using the convenient abbreviations built into PS1 (like \u for the 
# username), we have to use environment variables and other, less-reliable, ways
# of getting the info.
# See http://www.faqs.org/docs/Linux-HOWTO/Bash-Prompt-HOWTO.html for more info
# and http://www.dotfiles.com/ for more nice scripts like this.
function prompt_command 
{
	#Don't show the whole path if it's too long; this function from the HOWTO
	#How many characters of the $PWD should be kept
	local pwdmaxlen=28
	#Indicator that there has been directory truncation:
	local trunc_symbol="..."
	if [ ${#PWD} -gt $pwdmaxlen ]
	then
		local pwdoffset=$(( ${#PWD} - $pwdmaxlen ))
		newPWD="${trunc_symbol}${PWD:$pwdoffset:$pwdmaxlen}"
	else
		newPWD=${PWD}
	fi
	#Show the username and host (with colors defined above)
	echo -en "$USER_MODE$USER$HOST_MODE@"
	echo -n `hostname`
	#Show the CPU usage
	# echo -en " $CPU_MODE$(temp=$(cat /proc/loadavg) && echo ${temp%% *})"
	#Show the current directory (or most of it), and say how many files 
	#are in it and how big they are
	# echo -e " $DIR_MODE$newPWD $(ls -l | grep "^-" | wc -l | tr -d " ") visible files using$(ls --si -s | head -1 | perl -pe s#total##)$NORM_MODE"
}

# do ". acd_func.sh"
# acd_func 1.0.5, 10-nov-2004
# petar marinov, http:/geocities.com/h2428, this is public domain

cd_func ()
{
  local x2 the_new_dir adir index
  local -i cnt

  if [[ $1 ==  "--" ]]; then
    dirs -v
    return 0
  fi

  the_new_dir=$1
  [[ -z $1 ]] && the_new_dir=$HOME

  if [[ ${the_new_dir:0:1} == '-' ]]; then
    #
    # Extract dir N from dirs
    index=${the_new_dir:1}
    [[ -z $index ]] && index=1
    adir=$(dirs +$index)
    [[ -z $adir ]] && return 1
    the_new_dir=$adir
  fi

  #
  # '~' has to be substituted by ${HOME}
  [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

  #
  # Now change to the new dir and add to the top of the stack
  pushd "${the_new_dir}" > /dev/null
  [[ $? -ne 0 ]] && return 1
  the_new_dir=$(pwd)

  #
  # Trim down everything beyond 11th entry
  popd -n +11 2>/dev/null 1>/dev/null

  #
  # Remove any other occurence of this dir, skipping the top of the stack
  for ((cnt=1; cnt <= 10; cnt++)); do
    x2=$(dirs +${cnt} 2>/dev/null)
    [[ $? -ne 0 ]] && return 0
    [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
    if [[ "${x2}" == "${the_new_dir}" ]]; then
      popd -n +$cnt 2>/dev/null 1>/dev/null
      cnt=cnt-1
    fi
  done

  return 0
}


function pdfex()
{
    # this function uses 2 arguments:
    #     $1 is the page to extract
    #     $2 is the input file
    #     output file will be named "inputfile_pXX-pYY.pdf"
    gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
       -dFirstPage=${1} \
       -dLastPage=${1} \
       -sOutputFile=${2%.pdf}_p${1}.pdf \
       ${2}
}

function pdfpp()
{
    # this function uses 3 arguments:
    #     $1 is the first page of the range to extract
    #     $2 is the last page of the range to extract
    #     $3 is the input file
    #     output file will be named "inputfile_pXX-pYY.pdf"
    gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
       -dFirstPage=${1} \
       -dLastPage=${2} \
       -sOutputFile=${3%.pdf}_p${1}-p${2}.pdf \
       ${3}
}


# using MacPorts versions of coreutils ("g" prefix for gnu stuff)
if [ "$TERM" != "dumb" ]; then
    export LS_OPTIONS='--color=auto -hF'
    eval `gdircolors ~/.dir_colors`
fi
alias cd=cd_func
alias ls="gls $LS_OPTIONS"
alias ll="gls $LS_OPTIONS -l"
alias la="gls $LS_OPTIONS -a"
alias l="gls $LS_OPTIONS -lA"
alias lal="gls $LS_OPTIONS -la"

# alias mysql=/usr/local/mysql/bin/mysql
# alias mysqladmin=/usr/local/mysql/bin/mysqladmin

alias antlr4='java -jar /usr/local/jar/antlr-4.2-complete.jar'
alias grun='java org.antlr.v4.runtime.misc.TestRig'

alias emd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
alias emn='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c --no-wait'

if [[ $BASH_VERSION > "2.05a" ]]; then
  # ctrl+w shows the menu
  bind -x "\"\C-w\":cd_func -- ;"
fi

#
# Export Variables
#
# export PROMPT_COMMAND=prompt_command
export PS1="\w\n\\$ "

## Postgresql
#export PGPORT=5432

# Fedora repository
#export FEDORA_HOME=/Application/fedora32/


# rvm = ruby pkg mgr
# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
