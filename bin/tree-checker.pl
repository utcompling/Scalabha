#!/usr/bin/perl -w

# Author: Ulf Hermjakob
# Date: August 31, 2011

sub usage {
   print STDERR "$0 [-tok token_filename] < tree_filename > message_filename\n";
   print STDERR "   Tagset and error statistics are written to STDERR\n";
   print STDERR "   Optional output token_filename for tokenized text\n";
}

*STAT = *STDERR;
*MESS = *STDOUT;

%ht = ();
$o_tok_text_filename = "";

while (@ARGV) {
   $arg = shift @ARGV;
   if ($arg =~ /^-+tok/) {
      $o_tok_text_filename = shift @ARGV || ""
   } elsif ($arg =~ /^-+(h|help)$/i) {
      &usage;
      exit 1;
   } else {
      print STDERR "Ignoring unrecognized argument $arg\n";
   }
}

open(TOK, ">$o_tok_text_filename") || die "Can't write to $o_tok_text_filename" if $o_tok_text_filename;

$n_ill_formed_lines = 0;
$line_number = 0;
while (<>) {
   $line_number++;
   if ($line_number =~ /0000$/) {
      if ($line_number =~ /00000$/) {
	 print STDERR $line_number;
      } else {
	 print STDERR ".";
      }
   }

   if ($_ =~ /^\s*$/) {
      $core_message = "Empty line";
      print MESS "$core_message in line $line_number\n";
      $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
      print TOK "\n" if $o_tok_text_filename;
      $n_ill_formed_lines++;
      next;
   }

   @tokens = ();
   $nesting_level = 0;
   $n_units_at_top_level = 0;
   $orig_s = $_;
   $s = $orig_s;
   $s =~ s/^\s*//;
   $s =~ s/\s*$//;
   $s =~ s/ +/ /g;
   $rest = "";
   $core_message = "";

   while ($s =~ /\S/) {
      if (($tag, $token, $rest) = ($s =~ /^\s*\(([^ ()]+)\s+([^ ()]+)\)(.*)$/)) {
         $ht{TAG}->{$tag} = ($ht{TAG}->{$tag} || 0) + 1;
         $token = "(" if $token eq "-LRB-";
         $token = ")" if $token eq "-RRB-";
         push(@tokens, $token);
	 unless ($token =~ /^([\x00-\x7F]|[\xC0-\xDF][\x80-\xBF]|[\xE0-\xEF][\x80-\xBF]{2,2}|[\xF0-\xF7][\x80-\xBF]{3,3})*$/) {
            $core_message = "Token does not conform to UTF-8";
            print MESS "$core_message in line $line_number" . ": $token\n";
            $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
	 }
	 unless ($tag =~ /^[\x21-\x7E]*$/) {
            $core_message = "Tag is not regular ASCII";
            print MESS "$core_message in line $line_number" . ": $tag\n";
            $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
	 }
      } elsif (($bad_leaf, $rest) = ($s =~ /^\s*(\([^()]*\))(.*)$/)) {
         $core_message = "Ill-formed leaf token-text pair";
         print MESS "$core_message in line $line_number" . ": $bad_leaf\n";
         $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
      } elsif (($tag, $rest) = ($s =~ /^\s*\(([^ ()]+)\s+(\(.*\).*)$/)) { # )
         $ht{TAG}->{$tag} = ($ht{TAG}->{$tag} || 0) + 1;
         $nesting_level++; # (
      } elsif (($rest) = ($s =~ /^\s*\)(.*)$/)) {
         $nesting_level--;
	 if ($nesting_level == 0) {
            $n_units_at_top_level++;
	 } elsif ($nesting_level < 0) {
            $core_message = "Unexpected close parenthesis";
            print MESS "$core_message in line $line_number\n";
            $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
	 } 
      } else {
	 ($bad_token, $rest) = ($s =~ /^\s*(\S+)(|\s.*)$/);
	 $core_message = "Unexpected token";
	 print MESS "$core_message in line $line_number" . ": $bad_token\n";
	 $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
      }
      $s = $rest;
   }
   unless ($nesting_level == 0) {
      $core_message = "Missing close parenthesis";
      print MESS "$core_message in line $line_number\n";
      $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
   }
   if ($n_units_at_top_level > 1) {
      $core_message = "Multiple trees";
      print MESS "$core_message in line $line_number\n";
      $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
   }
   # if ($orig_s =~ /[()][()]/) {
   #    $core_message = "Missing space after parenthesis";
   #    print MESS "$core_message in line $line_number\n";
   #    $ht{ERROR}->{$core_message} = ($ht{ERROR}->{$core_message} || 0) + 1;
   # }
   $n_ill_formed_lines++ if $core_message;
   if ($o_tok_text_filename) {
      print TOK join(" ", @tokens) . "\n";
   }
}

close(TOK) if $o_tok_text_filename;

print STAT "\nTag statistics:\n";
foreach $tag (sort keys %{$ht{TAG}}) {
   $count = $ht{TAG}->{$tag};
   print STAT "   $tag: $count\n";
}
print STAT "\nError/warning statistics:\n";
foreach $error (sort keys %{$ht{ERROR}}) {
   $count = $ht{ERROR}->{$error};
   print STAT "   $error: $count\n";
}
print STAT "\n$n_ill_formed_lines of $line_number lines ill-formed.\n";

