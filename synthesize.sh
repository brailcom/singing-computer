#!/bin/bash

usage () {
  echo "usage: $0 FILE.xml [ LANGUAGE-CODE-OR-VOICE [ SPEEDUP ] ]"
  echo "       $0 FILE.ly [ LANGUAGE-CODE-OR-VOICE ]"
  exit 1
}

process_xml () {
  local file="$1"
  local voice="$2"
  local speedup="$3"
  local wave="${file%xml}wav"
  local tmpwave="$wave.tmp.$$"
  local tmpxml="$file.tmp.$$"
  local coding

  trap "rm -f '$tmpwave' '$tmpxml'" EXIT
  if [ "$voice" = voice_czech_ph ]; then
    coding=iso-8859-2
  else
    coding=iso-8859-1
  fi
  iconv -f utf-8 -t $coding -o "$tmpxml" "$file"
  text2wave -eval "($voice)" -mode singing "$tmpxml" -o "$tmpwave"
  if [ -n "$speedup" ] && [ $speedup -ne 1 ]; then
    sox "$tmpwave" "$wave" speed $speedup
  else
    mv "$tmpwave" "$wave"
  fi
  echo "$wave created."
}

process_ly () {
  local lyfile="$1"
  local voice="$2"

  lilypond "$lyfile"
  local xmlfile=$(ls -t $(dirname "$lyfile") | grep '\.xml$' | head -1)
  if [ -z "$xmlfile" ]; then
    echo "No XML file found."
    exit 1
  fi

  speedup=$(awk '/^#\(set! song:\*base-octave-shift\* -?[0-9]+\)/ { shift=$3 } END { print exp(-shift) }' "$lyfile")
  process_xml "$xmlfile" "$voice" $speedup
}

file="$1"
voice="$2"
speedup="$3"

if [ $# -lt 1 ] || [ $# -gt 3 ]; then
  usage
fi

if [ -z "$voice" ] || [ "$voice" = "en" ]; then
  voice=voice_kal_diphone
elif [ "$voice" = "cs" ]; then
  voice=voice_czech_ph
fi

if [ "$file" = "${file%ly}" ]; then
  process_xml "$file" "$voice" "$speedup"
else
  if [ -n "$speedup" ]; then
    usage
  fi
  process_ly "$file" "$voice"
fi

exit 0
