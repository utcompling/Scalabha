#!/usr/bin/perl -w

# Author: Ulf Hermjakob
# Usage: tokenize-text.pl [-{eng|fra|kin|mlg}] [-ssp] < IN > OUT
# Last updated: Sept. 9, 2011

$title_abbr_mc = "Adm|Amb|Brig|Capt|Col|Cpt|Dj|Dr|Eng|Fr|Gen|Gov|Hon|Ing|Ir|Jen|Jr|Lt|Maj|Messrs|Mr|Mrs|Ms|Mt|Pres|Pr|Prof|Rep|Rev|R\xC3\xA9v|Sen|Sgt|Spt|Sr|St|Sup|Supt";
$title_abbr_uc = uc $title_abbr_mc;
$general_abbr = "Jan|Feb|Febr|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|al|art|arts|cf|chap|cit|doc|ed|etc|ext|loc|no|nos|op|p|para|paras|pp|rev|sect|sects|seq|sq|ss|tel|v|vs|viz|vol|Art|Arts|Calif|Cf|Chap|Co|Col|Distr|Fla|Ibid|Inc|Ltd|No|Nos|Para|Prog|Rep|Rev|Sect|Vol|NO";
$general_abbr_uc = uc $general_abbr;
$mlg_bible_books = "Dan|Deo|Eks|Gen|Hab|Hag|Hos|Isa|Iza|Jer|Joe|Jos|Lev|Lio|Mal|Mat|Mik|Mpan|Oha|Sal|Sam|Zak";

$token_only_field = "";
$langcode3 = "";
$snt_split_prep_p = 0;
$snt_split_only_p = 0;

while (@ARGV) {
   $arg = shift @ARGV;
   if ($arg =~ /^-*(eng|fra|kin|mlg)$/i) {
      $langcode3 = lc $arg;
      $langcode3 =~ s/^-*//;
      # print STDERR "Language code: $langcode3\n";
   } elsif ($arg =~ /^-*ssp$/o) {
      $snt_split_prep_p = 1;
   } elsif ($arg =~ /^-*sso$/o) {
      $snt_split_only_p = 1;
      $snt_split_prep_p = 1;
   } elsif ($arg =~ /^::/) {
      $token_only_field = $arg;
      # print STDERR "token_only_field: $token_only_field\n";
   } else {
      print STDERR "Ignoring unrecognized argument $arg\n";
   } 
}

while(<>) {
   $line = $_;
   $line =~ s/^\s*//;
   $line =~ s/\s*$//;
   if (($token_only_field)
    && (($pre, $field, $post) = ($line =~ /^(.*$token_only_field\s+)(|\S|\S.*?\S)(\s*::.*)$/))) {
      $line = $field;
   } else {
      $pre = "";
      $post = "";
   }
   $line = " $line ";
   unless ($snt_split_only_p) {
      # comma etc., quotation marks, dollar, Euro, pound, parentheses, brackets, ellipsis, not sign, right arrow
      # registered/trademark sign
      $line =~ s/([;(){}\$\[\]]|\xE2\x80[\x98-\x9F\xA6]|\xE2\x82\xAC|\xE2\x84\xA2|\xE2\x86\x92|\xC2[\xA3\xAB\xAC\xAE\xBB])/ $1 /g;
      $line =~ s/(\S)([,"])/$1 \@$2/g;    # split with @ marker
      $line =~ s/([,"])(\S)/$1\@ $2/g;
      $line =~ s/([,"])([^ @])/$1\@ $2/g;
      $line =~ s/ \@"\@ " / \@" \@" /g;   # change: "" -> @"@ " -> @" @"
      # print "A: $line\n";
      $line =~ s/(\S)([?!][!?.]*) /$1 $2 /g;  # question and exclamation mark
      # watch out for %20 etc. in URLs
      $line =~ s/(\d)(%|\xE2\x80\xB0|\xC2[\xB0\xBA][CF])([^0-9a-fA-F])/$1 $2$3/g; # percent, per mille, degree C, degree F
      $line =~ s/\b(n\xC2\xB0)(\d)/$1 $2/gi;         # n+degree ("number sign")
      $line =~ s/([a-z0-9])(:) /$1 \@$2 /gi; # colon
      $line =~ s/('|\!|\?)(\.+) /$1 $2 /g;

      # English '
      $line =~ s/([a-z])('s)\b/$1 $2/gi;
      $line =~ s/([a-z]+s)(') /$1 $2 /gi if $langcode3 =~ /eng/;
      $line =~ s/\b(are|could|dare|did|do|does|had|has|have|is|must|need|should|was|were|would)(n)'(t)\b/$1 $2&nbt;'$3/gi; # protect n't from further tokenization
      $line =~ s/\b(can)(not)\b/$1 $2/gi;
      $line =~ s/\b(ca)(n)'(t)\b/$1$2 $2&nbt;'$3/gi;
      $line =~ s/\b([Ww])on't\b/$1ill n&nbt;'t/g;
      $line =~ s/\bWON'T\b/WILL N&nbt;'T/g;
      $line =~ s/\bain't\b/is n&nbt;'t/g;
      $line =~ s/\bAin't\b/Is n&nbt;'t/g;
      $line =~ s/\bAIN'T\b/IS N&nbt;'T/g;
      $line =~ s/\b(I)'(m)\b/$1 '$2/gi;
      $line =~ s/\b(I|you|he|she|we|they)'(d|ll|ve)\b/$1 '$2/gi;
      $line =~ s/\b(you|we|they)'(re)\b/$1 '$2/gi;
      if ($langcode3 =~ /eng/) {
	 $line =~ s/([A-Z]) '(D|LL|M|RE|S|T|VE)\b/$1 &apos;$2/g; # protect THEY 'VE
         $line =~ s/ '([A-Z]|[a-z]{3,}|to\b)/ \xE2\x80\x98 $1/g;  # left single quotation mark
         $line =~ s/([a-z])([.!?])' /$1 $2 \xE2\x80\x99 /g;  # right single quotation mark
         $line =~ s/ ([.!?])' / $1 \xE2\x80\x99 /g;          # right single quotation mark
         $line =~ s/([a-z]{2,})' /$1 \xE2\x80\x99 /gi;       # right single quotation mark
         $line =~ s/ (\@,\@) ' / $1 \xE2\x80\x99 /gi;        # right single quotation mark
	 $line =~ s/&apos;/'/g;
      }
      $line =~ s/(\d) \@,\@ (\d)/$1,$2/g; # repair: number,number
      $line =~ s/(\d) \@,\@ (\d)/$1,$2/g;
      $line =~ s/ \@,/ ,/g;               # remove @ marker from ,
      $line =~ s/,\@ /, /g;

      # French '
      $line =~ s/\b(d'|l')([aehiou]|\xC3[\x89\xA9])/$1 $2/gi;
      $line =~ s/\b(c')(est)\b/$1 $2/gi;

      # Kinyarwanda '
      if ($langcode3 =~ /kin/) {
         foreach $i ((1 .. 2)) {
            $line =~ s/\b(ab|ah|ak|ay|b|bw|by|cy|h|iby|icy|iry|iy|iz|k|kubw|kw|m|mur|mw|n|ng|nk|nt|ny|nyir|rw|ry|ubw|ukw|urw|utw|uw|tw|w|y|z)'([aeiou])/$1' $2/gi if $i;
	  $line =~ s/\b(n)'([bdgkmtz])/$1' $2/gi;
	  $line =~ s/\b([bcr]?y)'([bgmns])/$1' $2/gi;
          $line =~ s/\b([a-z]{2,}[bhkmnrwyz]')((?:aba|aga|aha|aka|ama|i|ubu|ubw|udu|ugu|uko|uku|ukw|umu|umw|uru|urw|utu|utw)[a-z]*)\b/$1 $2/gi;
         }
         $line =~ s/(\d)(fr|frs|frw)\b/$1 $2/gi; # Rwanda Francs (currency)
         $line =~ s/\b(frw)(\d)/$1 $2/gi;
         # centr'afu?rika
      }

      # Malagasy
      if ($langcode3 =~ /mlg/) {
         $line =~ s/([bdfghjklmnprstvz]+')([aeio]|\xC3\[\xA0\xA8\xAC\xB2\xB4]|\xE1\xBB\xB3)/$1 $2/gi;
         $line =~ s/(n')(ny|ilay)\b/$1 $2/gi; # article
         $line =~ s/((?:ak|n|tr)')([A-Z])/$1 $2/g;
      }
   
      $line =~ s/(\d)(cm|g|ha|kg|km|Km|kwh|Kwh|m|mg|mm)\b/$1 $2/g; # gram and other measures

      # no-break space, narrow no-break space
      $line =~ s/ (\d{1,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3}) / $1.$2 /g;
      $line =~ s/ (\d{1,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3}) / $1.$2.$3 /g;
      $line =~ s/ (\d{1,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3})(?:\xC2\xA0|\xE2\x80\xAF)(\d{3,3}) / $1.$2.$3.$4 /g;
      $line =~ s/(\xC2\xA0|\xE2\x80\xAF)+ / /g;
      $line =~ s/ (\xC2\xA0|\xE2\x80\xAF)+/ /g;
      $line =~ s/([a-z])(?:\xC2\xA0|\xE2\x80\xAF)+([a-z])/$1 $2/gi;

      # hyphen, slash, colon
      $line =~ s/ (::[a-z][-_a-z0-9]*[a-z0-9]|end-of-article) / &nbt;$1&nbt; /g; # no-break-token
      $line =~ s/ (::[a-z][-_a-z0-9]*[a-z0-9]|end-of-article) / &nbt;$1&nbt; /g; # no-break-token
      $prev_version = "";
      while ($line ne $prev_version) {
         $prev_version = $line;
         $line =~ s/ (\d+|\d{1,3}(?:\.\d{3,3})+)(-)(\d+|\d{1,3}(?:\.\d{3,3})+) / $1 \@$2\@ $3 /g;
         $line =~ s/ (\d+|\d{1,3}(?:,\d{3,3})+)(-)(\d+|\d{1,3}(?:,\d{3,3})+) / $1 \@$2\@ $3 /g;
         $line =~ s/ ((?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)+)(-|:|\/)((?:[-\/a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)+'?\.*) / $1 \@$2\@ $3 /gi;
         $line =~ s/ ((?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)+|-?\d+(?:[.,]\d+)*)(-|:|\/|\*+|\++) / $1 \@$2 /gi;
         $line =~ s/ (-|:|\/|\*+|\++)((?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)+\.*) / $1\@ $2 /gi;
      }
      $line =~ s/ (Foto)\/([A-Z])/ $1 \@\/\@ $2/g;
      $line =~ s/ ([a-zA-Z]+)-((?:[A-Z]|Dr|Prof|St)\.)([A-Z][a-z]+) / $1 \@-\@ $2 $3 /g;
      $line =~ s/ ($mlg_bible_books)\.(\d+\.)/ $1. $2/g if $langcode3 =~ /mlg/;

      $line =~ s/ ((?:[0-2]?[1-9]|3[01])\/(?:0?[1-9]|1[0-2])\/) ((?:19|20)\d\d) / $1$2 /g; # join date 28/2/ 2011

      # period
      $line =~ s/ +/ /g;
      $line =~ s/ (www)\. ([a-z]{3,}) / $1.$2 /gi;
      $line =~ s/ ((?:$title_abbr_mc)\.)([A-Z][a-z]+) / $1 $2 /g;
      $line =~ s/ ([A-Z]\.)([A-Z](?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3){3,}) / $1 $2 /g;
      $line =~ s/ ([a-zA-Z][a-z]{3,}|[A-Z]{3,}|\d+|\d{1,3}(?:\.\d{3,3})+|\d{1,3}\.\d{1,3})\.([A-Z][a-z]+'?) / $1. $2 /g;
      $line =~ s/ (Brig|Col|Lt)\.(Gen|Jen)\. / $1. $2. /gi;
      $line =~ s/ (Gen|Jen)\.(Maj)\. / $1. $2. /gi;
      $line =~ s/ (Prof|Pr)\.(Dr)\. / $1. $2. /gi;
      $line =~ s/([ \/][A-Z]|\b$title_abbr_mc|\b$title_abbr_uc)\. ([A-Z])/$1.&nbsp;$2/g;
      $line =~ s/([ \/][A-Z]|\b$title_abbr_mc|\b$title_abbr_uc)\. ([A-Z])/$1.&nbsp;$2/g;
      $line =~ s/([ \/][A-Z]|\b$title_abbr_mc|\b$title_abbr_uc)\.\s*$/$1&nbt;./;
      $line =~ s/^((?:|.*[ \/][A-Z]))\.\s*$/$1&nbt;./; # N.
      $line =~ s/^(\s*[A-Z])\.\s*$/$1&nbt;./;
      $line =~ s/\b($general_abbr)\.\s+/$1&nbt;. /g;
      $line =~ s/\b($general_abbr)\.$/$1&nbt;./;
      $line =~ s/\b($general_abbr_uc)\.\s+/$1&nbt;. /g unless $line =~ /[a-z]/;
      $line =~ s/\b($general_abbr_uc)\.$/$1&nbt;./ unless $line =~ /[a-z]/;
      $line =~ s/\b((?:[A-Za-z]\.){1,3}[A-Za-z])\.\s+/$1&nbt;. /g; # e.g. a.m. O.B.E.
      $line =~ s/\b((?:[A-Za-z]\.){1,3}[A-Za-z])\.$/$1&nbt;./; # e.g. a.m. O.B.E.
      $line =~ s/(\b(?:$mlg_bible_books))\. (\d+[.:])/$1.&nbsp;$2/g if $langcode3 =~ /mlg/;
      $line =~ s/((?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3){5,5})\. /$1 . /g;
      $line =~ s/([a-z][aeiou][a-z][aeiou])\. /$1 . /gi;
      $line =~ s/(\xC3[\xA0\xA8\xAC\xB2\xB4]|\xE1\xBB\xB3)\. /$1 . /gi; # Malagasy vowels
      $line =~ s/([aeio][bdfghjklmnprstvz][aeio]?[aeioy])\. /$1 . /g; # Malagasy
      $line =~ s/ (\d+|(?:[a-z]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)+)\. *((?:(?:"|\@"|\xC2\xBB|\xE2\x80[\x99\x9D]) )?(?:|(?:&nbt;)?::.*))$/ $1 . $2/i;
      $line =~ s/ ((?:19|20)\d\d)\. ((?:(?:"|\@"|\xC2\xAB|\xC2\xBB|\xE2\x80[\x98\x99\x9C\x9D]) )?[A-Z])/ $1 . $2/g;
      $line =~ s/(\S) (-?\d+|\d+[-\/:]\d+|\d{1,3}\.\d{1,3}|\d{1,3}(?:\.\d{3,3})+|\d{1,3},\d{1,3}|\d{1,3}(?:,\d{3,3})+|(?:\d\d?-)?\d\d?\/\d\d?(?:\/\d\d\d\d)?|[a-zA-Z]\d+)\. ((?:(?:"|\@"|\xC2\xAB|\xC2\xBB|\xE2\x80[\x98\x99\x9C\x9D]) )?(?:[A-Z]|::|&nbt;::))/$1 $2 . $3/g;
      $line =~ s/(\S) (\S+[aeiou]|[a-z]\S+[a-z]|[A-Z]{2,}|frw|Frw|[A-Z]\S*[a-z][A-Za-z\x80-\xFF])\. ((?:(?:"|\(|\)|\@"|\xC2\xAB|\xC2\xBB|\xE2\x80[\x98\x99\x9C\x9D]) )?(?:|[A-Z]|::|&nbt;::))/$1 $2 . $3/g;
      $line =~ s/(\S) (am|at|by|do|in|is|it|of|on|so|to|us)\. ((?:(?:"|\(|\)|\@"|\xC2\xAB|\xC2\xBB|\xE2\x80[\x98\x99\x9C\x9D]) )?(?:|[A-Z]|::|&nbt;::))/$1 $2 . $3/g if $langcode3 =~ /eng/;
      $line =~ s/(%|\xC2[\xB2\xBC-\xBD]|\xC2\xB0[CF]|\xC2\xBA[CF])\./$1 ./g;
      $line =~ s/ \.([A-Z])/ . $1/g;
      $line =~ s/ ([a-z0-9&':]*[0-9&':][a-z0-9&':]*[a-z])\. / $1 . /gi;
      $line =~ s/ ([-a-z0-9&':+%\/=]*[a-z&':+%][-a-z0-9&':+%\/=]*[0-9])\. / $1 . /gi;
      $line =~ s/ ([-0-9.,\/]*\d[.,\/]\d[-0-9.,\/]*)\. / $1 . /g;
      $line =~ s/([a-z0-9]|\xC3[\xA0-\xBF]|\xE1\xBB\xB3)(\.{2,}) /$1 $2 /gi;
      $line =~ s/ (\.{2,})([a-z0-9])/ $1 $2/gi;
      $line =~ s/ ([!?]+|\++|\#+|')\. / $1 . /g;
      $line =~ s/ ([a-z]+-+)\. / $1 . /gi;
      $line =~ s/ (KIST)\. / $1 . /g; # Period not part of abbreviation as for USC.
      if ($langcode3 =~ /mlg/) {
         $line =~ s/ (Aho|Ary|Ay|Esao|Indro|Ireo|Syria)\. / $1 . /gi;
         $line =~ s/ (e|ny|omby|roa|teo)\. / $1 . /g;
      } elsif ($langcode3 =~ /eng/) {
         $line =~ s/ (all|asks?|back|barn|book|bow|days?|dead|die|down|dug|dust|ear|else|envy|food|go|gods?|gold|hair|hand|he|heed|help|her|hill|him|king|land|law|lips?|man|me|mind|near|new|noon|oil|old|ox|poor|rest|roof|said|says?|sea|seen|ship|shut|sins?|sky|sons?|that|them|this|tomb|too|up|wait|wall|war|way|wear|you)\. / $1 . /g;
         $line =~ s/ (Amen|David|Egypt|God|John|Judah|Lord|Moses|Peter|Ruth|Saul|Sodom)\. / $1 . /gi;
      }

      # period (join)
      $line =~ s/ (www\.) ((?:[a-z]{2,}\.)+(?:com|edu|fr|net|org|rw)) / $1$2 /gi;

      # URL + other stuff
      $line =~ s/((?:www\.|http:).\S*[a-z])([-.'\/:]) /$1 $2 /gi;
      $line =~ s/([a-z])([-\/:])((?:www\.|http:).\S*) /$1 \@$2\@ $3 /gi;

      $line =~ s/&nbsp;/ /g;
      $line =~ s/&nbt;//g;
   }

   if ($snt_split_only_p || $snt_split_prep_p) {
      $line =~ s/ ([.?!](?: (?:\)|\@"|\xC2\xBB|\xE2\x80\x99|\xE2\x80\x9D|\xE2\x80\xA6))?) ((?:(?:\(|"\@|\xC2\xAB|\xE2\x80\x98|\xE2\x80\x9C) )?[A-Z0-9])/ $1 <EOS> $2/g;
   }

   $line = "$pre $line $post";
   $line =~ s/\s+/ /g;
   $line =~ s/^\s+//;
   $line =~ s/\s+$//;
   print "$line\n";
}
