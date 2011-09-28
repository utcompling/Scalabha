#!/usr/bin/perl -w

# Author: Ulf Hermjakob
# Usage: normalize-text-standalone.pl [-{eng|fra|kin|mlg}] < IN > OUT
# Last updated: Sept. 9, 2011

# convert html-style characters such as &amp; &#38; &#x26; to UTF-8 &
sub unguard_html_r {
   local($string) = @_;

   return undef unless defined($string);

   $string =~ s/&amp;/&/g;
   $string =~ s/&quot;/'/g;
   $string =~ s/&lt;/</g;
   $string =~ s/&gt;/>/g;

   ($d) = ($string =~ /&#(\d+);/);
   while (defined($d)) {
      $c = &chr($d);
      $string =~ s/&#$d;/$c/g;
      ($d) = ($string =~ /&#(\d+);/);
   }
   ($x) = ($string =~ /&#x([0-9a-f]+);/i);
   while (defined($x)) {
      $c = &chr(hex($x));
      $string =~ s/&#x$x;/$c/g;
      ($x) = ($string =~ /&#x([0-9a-f]+);/i);
   }
   ($x) = ($string =~ /(?:http|www|\.com)\S*\%([0-9a-f]{2,2})/i);
   while (defined($x)) {
      $c = &chr("%" . hex($x));
      $string =~ s/\%$x/$c/g;
      ($x) = ($string =~ /(?:http|www|\.com)\S*\%([0-9a-f]{2,2})/i);
   }
   return $string;
}

# convert unicode number to UTF-8 character
sub chr {
   local($i) = @_;

   return undef unless $i =~ /^\%?\d+$/;
   if ($i =~ /^%/) {
      $i =~ s/^\%//;
      return chr($i)                                  if $i < 128;
      return "\x80" | chr($i - 128)                   if $i < 256;
   } else {
      return chr($i)                                  if $i < 128;
      return ("\xC0" | chr(($i / 64) % 32)) 
	   . ("\x80" | chr($i % 64))                  if $i < 2048;
      return ("\xE0" | chr(int($i / 4096) % 16)) 
	   . ("\x80" | chr(int($i / 64) % 64)) 
	   . ("\x80" | chr($i % 64))                  if $i < 65536;
      return ("\xF0" | chr(int($i / 262144) % 8)) 
	   . ("\x80" | chr(int($i / 4096) % 64)) 
	   . ("\x80" | chr(int($i / 64) % 64)) 
	   . ("\x80" | chr($i % 64))                  if $i < 2097152;
   }
   return "?";
}

$langcode3 = "";

# process args
while (@ARGV) {
   $arg = shift @ARGV;
   if ($arg =~ /^-*(eng|fra|kin|mlg)$/i) {
      $langcode3 = lc $arg;
      $langcode3 =~ s/^-*//;
      # print STDERR "Language code: $langcode3\n";
   } else {
      print STDERR "Ignoring unrecognized argument $arg\n";
   } 
}

while(<>) {
   $line = $_;
   $line =~ s/([knrwyz])\x1A([aeiou])/$1'$2/ig            # U+001A substitute -> '
      if $langcode3 eq "kin";
   $line =~ s/[\x00-\x07\x0E-\x1F\x7F]//g;                # control characters (C0)
   $line =~ s/\xC2[\x80-\x9F]//g;                         # control characters (C1)

   $line =~ s/ &quot;([a-z])/ \xE2\x80\x98 $1/gi;         # left single quotation mark
   $line =~ s/((?:[a-z]|\xC3[\xB4])[,;.?!]?)&quot;([,;.?!]?) /$1 \xE2\x80\x99 $2 /gi; # right single quotation mark
   $line = &unguard_html_r($line);                        # &amp; &#964;
   $line =~ s/\xEF\x82\xA7/\xE2\x96\xA1/g;                # undefined U+F047 -> white square
   $line =~ s/\xC2\xA8/"/g;                               # diaeresis -> "
   $line =~ s/\xCB\x9D/"/g;                               # double acute accent -> "
   if ($langcode3 =~ /^(eng|fra|kin|mlg)$/) {
      $line =~ s/\xE1\xBB\xA7/\xC3\xB9/g;                 # U+1EEF u with hook -> u grave
      $line =~ s/([aeou])\xD1\x97([a-z])/$1\xC3\xAF$2/gi; # U+0457 cyrillic yi -> i diaerisis
   }
   $line =~ s/\xE2\x80\x98\xE2\x80\x98/\xE2\x80\x9C/g;    # two left  single -> one double quotation mark
   $line =~ s/(\xE2\x80\x98)([\x00-\xDF]*)(\xE2\x80\x99)\xE2\x80\x99\xE2\x80\x99/$1$2$3\xE2\x80\x9D/g; # `...'''
   $line =~ s/\xE2\x80\x99\xE2\x80\x99/\xE2\x80\x9D/g;    # two right single -> one double quotation mark
   $line =~ s/([a-z])\xE2\x80\x99([a-z])/$1'$2/ig;        # U+2019 right single quotation mark -> '
   $line =~ s/([a-z])\xC2\xB4([a-z])/$1'$2/ig;            # acute accent -> '
   $line =~ s/([a-z])`([a-z])/$1'$2/ig;                   # grave accent -> '
   $line =~ s/\b(ab|ah|ak|ay|b|bw|by|cy|h|iby|icy|iry|iy|iz|k|kubw|kw|m|mur|mw|n|nk|nt|ny|nyir|rw|ry|ubw|ukw|urw|utw|uw|tw|w|y|z)\xE2\x80\x99\s+([a-z])/$1' $2/ig
      if $langcode3 eq "kin";
   $line =~ s/\xE2\x80[\x80-\x8A]/ /g;                    # U+2000 or U+200A -> space
   $line =~ s/\xC2\xA0/ /g;                               # U+00A0 -> space
   $line =~ s/\xC2\xAD//g;                                # U+00AD soft hyphen -> delete
   $line =~ s/\xE2\x80\x93/-/g;                           # U+2013 EN dash -> -
   $line =~ s/\xE2\x80\x94/-/g;                           # U+2014 EM dash -> -
   $line =~ s/ +/ /g;
   print $line;
}

