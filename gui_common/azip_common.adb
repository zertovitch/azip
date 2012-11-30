with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body AZip_Common is

  -- http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT

  IBM_437_to_UTF_16: constant array(Character) of Wide_Character :=
  (
    Character'Val(16#00#) => Wide_Character'Val(16#0000#) , -- NULL
    Character'Val(16#01#) => Wide_Character'Val(16#0001#) , -- START OF HEADING
    Character'Val(16#02#) => Wide_Character'Val(16#0002#) , -- START OF TEXT
    Character'Val(16#03#) => Wide_Character'Val(16#0003#) , -- END OF TEXT
    Character'Val(16#04#) => Wide_Character'Val(16#0004#) , -- END OF TRANSMISSION
    Character'Val(16#05#) => Wide_Character'Val(16#0005#) , -- ENQUIRY
    Character'Val(16#06#) => Wide_Character'Val(16#0006#) , -- ACKNOWLEDGE
    Character'Val(16#07#) => Wide_Character'Val(16#0007#) , -- BELL
    Character'Val(16#08#) => Wide_Character'Val(16#0008#) , -- BACKSPACE
    Character'Val(16#09#) => Wide_Character'Val(16#0009#) , -- HORIZONTAL TABULATION
    Character'Val(16#0a#) => Wide_Character'Val(16#000a#) , -- LINE FEED
    Character'Val(16#0b#) => Wide_Character'Val(16#000b#) , -- VERTICAL TABULATION
    Character'Val(16#0c#) => Wide_Character'Val(16#000c#) , -- FORM FEED
    Character'Val(16#0d#) => Wide_Character'Val(16#000d#) , -- CARRIAGE RETURN
    Character'Val(16#0e#) => Wide_Character'Val(16#000e#) , -- SHIFT OUT
    Character'Val(16#0f#) => Wide_Character'Val(16#000f#) , -- SHIFT IN
    Character'Val(16#10#) => Wide_Character'Val(16#0010#) , -- DATA LINK ESCAPE
    Character'Val(16#11#) => Wide_Character'Val(16#0011#) , -- DEVICE CONTROL ONE
    Character'Val(16#12#) => Wide_Character'Val(16#0012#) , -- DEVICE CONTROL TWO
    Character'Val(16#13#) => Wide_Character'Val(16#0013#) , -- DEVICE CONTROL THREE
    Character'Val(16#14#) => Wide_Character'Val(16#0014#) , -- DEVICE CONTROL FOUR
    Character'Val(16#15#) => Wide_Character'Val(16#0015#) , -- NEGATIVE ACKNOWLEDGE
    Character'Val(16#16#) => Wide_Character'Val(16#0016#) , -- SYNCHRONOUS IDLE
    Character'Val(16#17#) => Wide_Character'Val(16#0017#) , -- END OF TRANSMISSION BLOCK
    Character'Val(16#18#) => Wide_Character'Val(16#0018#) , -- CANCEL
    Character'Val(16#19#) => Wide_Character'Val(16#0019#) , -- END OF MEDIUM
    Character'Val(16#1a#) => Wide_Character'Val(16#001a#) , -- SUBSTITUTE
    Character'Val(16#1b#) => Wide_Character'Val(16#001b#) , -- ESCAPE
    Character'Val(16#1c#) => Wide_Character'Val(16#001c#) , -- FILE SEPARATOR
    Character'Val(16#1d#) => Wide_Character'Val(16#001d#) , -- GROUP SEPARATOR
    Character'Val(16#1e#) => Wide_Character'Val(16#001e#) , -- RECORD SEPARATOR
    Character'Val(16#1f#) => Wide_Character'Val(16#001f#) , -- UNIT SEPARATOR
    Character'Val(16#20#) => Wide_Character'Val(16#0020#) , -- SPACE
    Character'Val(16#21#) => Wide_Character'Val(16#0021#) , -- EXCLAMATION MARK
    Character'Val(16#22#) => Wide_Character'Val(16#0022#) , -- QUOTATION MARK
    Character'Val(16#23#) => Wide_Character'Val(16#0023#) , -- NUMBER SIGN
    Character'Val(16#24#) => Wide_Character'Val(16#0024#) , -- DOLLAR SIGN
    Character'Val(16#25#) => Wide_Character'Val(16#0025#) , -- PERCENT SIGN
    Character'Val(16#26#) => Wide_Character'Val(16#0026#) , -- AMPERSAND
    Character'Val(16#27#) => Wide_Character'Val(16#0027#) , -- APOSTROPHE
    Character'Val(16#28#) => Wide_Character'Val(16#0028#) , -- LEFT PARENTHESIS
    Character'Val(16#29#) => Wide_Character'Val(16#0029#) , -- RIGHT PARENTHESIS
    Character'Val(16#2a#) => Wide_Character'Val(16#002a#) , -- ASTERISK
    Character'Val(16#2b#) => Wide_Character'Val(16#002b#) , -- PLUS SIGN
    Character'Val(16#2c#) => Wide_Character'Val(16#002c#) , -- COMMA
    Character'Val(16#2d#) => Wide_Character'Val(16#002d#) , -- HYPHEN-MINUS
    Character'Val(16#2e#) => Wide_Character'Val(16#002e#) , -- FULL STOP
    Character'Val(16#2f#) => Wide_Character'Val(16#002f#) , -- SOLIDUS
    Character'Val(16#30#) => Wide_Character'Val(16#0030#) , -- DIGIT ZERO
    Character'Val(16#31#) => Wide_Character'Val(16#0031#) , -- DIGIT ONE
    Character'Val(16#32#) => Wide_Character'Val(16#0032#) , -- DIGIT TWO
    Character'Val(16#33#) => Wide_Character'Val(16#0033#) , -- DIGIT THREE
    Character'Val(16#34#) => Wide_Character'Val(16#0034#) , -- DIGIT FOUR
    Character'Val(16#35#) => Wide_Character'Val(16#0035#) , -- DIGIT FIVE
    Character'Val(16#36#) => Wide_Character'Val(16#0036#) , -- DIGIT SIX
    Character'Val(16#37#) => Wide_Character'Val(16#0037#) , -- DIGIT SEVEN
    Character'Val(16#38#) => Wide_Character'Val(16#0038#) , -- DIGIT EIGHT
    Character'Val(16#39#) => Wide_Character'Val(16#0039#) , -- DIGIT NINE
    Character'Val(16#3a#) => Wide_Character'Val(16#003a#) , -- COLON
    Character'Val(16#3b#) => Wide_Character'Val(16#003b#) , -- SEMICOLON
    Character'Val(16#3c#) => Wide_Character'Val(16#003c#) , -- LESS-THAN SIGN
    Character'Val(16#3d#) => Wide_Character'Val(16#003d#) , -- EQUALS SIGN
    Character'Val(16#3e#) => Wide_Character'Val(16#003e#) , -- GREATER-THAN SIGN
    Character'Val(16#3f#) => Wide_Character'Val(16#003f#) , -- QUESTION MARK
    Character'Val(16#40#) => Wide_Character'Val(16#0040#) , -- COMMERCIAL AT
    Character'Val(16#41#) => Wide_Character'Val(16#0041#) , -- LATIN CAPITAL LETTER A
    Character'Val(16#42#) => Wide_Character'Val(16#0042#) , -- LATIN CAPITAL LETTER B
    Character'Val(16#43#) => Wide_Character'Val(16#0043#) , -- LATIN CAPITAL LETTER C
    Character'Val(16#44#) => Wide_Character'Val(16#0044#) , -- LATIN CAPITAL LETTER D
    Character'Val(16#45#) => Wide_Character'Val(16#0045#) , -- LATIN CAPITAL LETTER E
    Character'Val(16#46#) => Wide_Character'Val(16#0046#) , -- LATIN CAPITAL LETTER F
    Character'Val(16#47#) => Wide_Character'Val(16#0047#) , -- LATIN CAPITAL LETTER G
    Character'Val(16#48#) => Wide_Character'Val(16#0048#) , -- LATIN CAPITAL LETTER H
    Character'Val(16#49#) => Wide_Character'Val(16#0049#) , -- LATIN CAPITAL LETTER I
    Character'Val(16#4a#) => Wide_Character'Val(16#004a#) , -- LATIN CAPITAL LETTER J
    Character'Val(16#4b#) => Wide_Character'Val(16#004b#) , -- LATIN CAPITAL LETTER K
    Character'Val(16#4c#) => Wide_Character'Val(16#004c#) , -- LATIN CAPITAL LETTER L
    Character'Val(16#4d#) => Wide_Character'Val(16#004d#) , -- LATIN CAPITAL LETTER M
    Character'Val(16#4e#) => Wide_Character'Val(16#004e#) , -- LATIN CAPITAL LETTER N
    Character'Val(16#4f#) => Wide_Character'Val(16#004f#) , -- LATIN CAPITAL LETTER O
    Character'Val(16#50#) => Wide_Character'Val(16#0050#) , -- LATIN CAPITAL LETTER P
    Character'Val(16#51#) => Wide_Character'Val(16#0051#) , -- LATIN CAPITAL LETTER Q
    Character'Val(16#52#) => Wide_Character'Val(16#0052#) , -- LATIN CAPITAL LETTER R
    Character'Val(16#53#) => Wide_Character'Val(16#0053#) , -- LATIN CAPITAL LETTER S
    Character'Val(16#54#) => Wide_Character'Val(16#0054#) , -- LATIN CAPITAL LETTER T
    Character'Val(16#55#) => Wide_Character'Val(16#0055#) , -- LATIN CAPITAL LETTER U
    Character'Val(16#56#) => Wide_Character'Val(16#0056#) , -- LATIN CAPITAL LETTER V
    Character'Val(16#57#) => Wide_Character'Val(16#0057#) , -- LATIN CAPITAL LETTER W
    Character'Val(16#58#) => Wide_Character'Val(16#0058#) , -- LATIN CAPITAL LETTER X
    Character'Val(16#59#) => Wide_Character'Val(16#0059#) , -- LATIN CAPITAL LETTER Y
    Character'Val(16#5a#) => Wide_Character'Val(16#005a#) , -- LATIN CAPITAL LETTER Z
    Character'Val(16#5b#) => Wide_Character'Val(16#005b#) , -- LEFT SQUARE BRACKET
    Character'Val(16#5c#) => Wide_Character'Val(16#005c#) , -- REVERSE SOLIDUS
    Character'Val(16#5d#) => Wide_Character'Val(16#005d#) , -- RIGHT SQUARE BRACKET
    Character'Val(16#5e#) => Wide_Character'Val(16#005e#) , -- CIRCUMFLEX ACCENT
    Character'Val(16#5f#) => Wide_Character'Val(16#005f#) , -- LOW LINE
    Character'Val(16#60#) => Wide_Character'Val(16#0060#) , -- GRAVE ACCENT
    Character'Val(16#61#) => Wide_Character'Val(16#0061#) , -- LATIN SMALL LETTER A
    Character'Val(16#62#) => Wide_Character'Val(16#0062#) , -- LATIN SMALL LETTER B
    Character'Val(16#63#) => Wide_Character'Val(16#0063#) , -- LATIN SMALL LETTER C
    Character'Val(16#64#) => Wide_Character'Val(16#0064#) , -- LATIN SMALL LETTER D
    Character'Val(16#65#) => Wide_Character'Val(16#0065#) , -- LATIN SMALL LETTER E
    Character'Val(16#66#) => Wide_Character'Val(16#0066#) , -- LATIN SMALL LETTER F
    Character'Val(16#67#) => Wide_Character'Val(16#0067#) , -- LATIN SMALL LETTER G
    Character'Val(16#68#) => Wide_Character'Val(16#0068#) , -- LATIN SMALL LETTER H
    Character'Val(16#69#) => Wide_Character'Val(16#0069#) , -- LATIN SMALL LETTER I
    Character'Val(16#6a#) => Wide_Character'Val(16#006a#) , -- LATIN SMALL LETTER J
    Character'Val(16#6b#) => Wide_Character'Val(16#006b#) , -- LATIN SMALL LETTER K
    Character'Val(16#6c#) => Wide_Character'Val(16#006c#) , -- LATIN SMALL LETTER L
    Character'Val(16#6d#) => Wide_Character'Val(16#006d#) , -- LATIN SMALL LETTER M
    Character'Val(16#6e#) => Wide_Character'Val(16#006e#) , -- LATIN SMALL LETTER N
    Character'Val(16#6f#) => Wide_Character'Val(16#006f#) , -- LATIN SMALL LETTER O
    Character'Val(16#70#) => Wide_Character'Val(16#0070#) , -- LATIN SMALL LETTER P
    Character'Val(16#71#) => Wide_Character'Val(16#0071#) , -- LATIN SMALL LETTER Q
    Character'Val(16#72#) => Wide_Character'Val(16#0072#) , -- LATIN SMALL LETTER R
    Character'Val(16#73#) => Wide_Character'Val(16#0073#) , -- LATIN SMALL LETTER S
    Character'Val(16#74#) => Wide_Character'Val(16#0074#) , -- LATIN SMALL LETTER T
    Character'Val(16#75#) => Wide_Character'Val(16#0075#) , -- LATIN SMALL LETTER U
    Character'Val(16#76#) => Wide_Character'Val(16#0076#) , -- LATIN SMALL LETTER V
    Character'Val(16#77#) => Wide_Character'Val(16#0077#) , -- LATIN SMALL LETTER W
    Character'Val(16#78#) => Wide_Character'Val(16#0078#) , -- LATIN SMALL LETTER X
    Character'Val(16#79#) => Wide_Character'Val(16#0079#) , -- LATIN SMALL LETTER Y
    Character'Val(16#7a#) => Wide_Character'Val(16#007a#) , -- LATIN SMALL LETTER Z
    Character'Val(16#7b#) => Wide_Character'Val(16#007b#) , -- LEFT CURLY BRACKET
    Character'Val(16#7c#) => Wide_Character'Val(16#007c#) , -- VERTICAL LINE
    Character'Val(16#7d#) => Wide_Character'Val(16#007d#) , -- RIGHT CURLY BRACKET
    Character'Val(16#7e#) => Wide_Character'Val(16#007e#) , -- TILDE
    Character'Val(16#7f#) => Wide_Character'Val(16#007f#) , -- DELETE
    Character'Val(16#80#) => Wide_Character'Val(16#00c7#) , -- LATIN CAPITAL LETTER C WITH CEDILLA
    Character'Val(16#81#) => Wide_Character'Val(16#00fc#) , -- LATIN SMALL LETTER U WITH DIAERESIS
    Character'Val(16#82#) => Wide_Character'Val(16#00e9#) , -- LATIN SMALL LETTER E WITH ACUTE
    Character'Val(16#83#) => Wide_Character'Val(16#00e2#) , -- LATIN SMALL LETTER A WITH CIRCUMFLEX
    Character'Val(16#84#) => Wide_Character'Val(16#00e4#) , -- LATIN SMALL LETTER A WITH DIAERESIS
    Character'Val(16#85#) => Wide_Character'Val(16#00e0#) , -- LATIN SMALL LETTER A WITH GRAVE
    Character'Val(16#86#) => Wide_Character'Val(16#00e5#) , -- LATIN SMALL LETTER A WITH RING ABOVE
    Character'Val(16#87#) => Wide_Character'Val(16#00e7#) , -- LATIN SMALL LETTER C WITH CEDILLA
    Character'Val(16#88#) => Wide_Character'Val(16#00ea#) , -- LATIN SMALL LETTER E WITH CIRCUMFLEX
    Character'Val(16#89#) => Wide_Character'Val(16#00eb#) , -- LATIN SMALL LETTER E WITH DIAERESIS
    Character'Val(16#8a#) => Wide_Character'Val(16#00e8#) , -- LATIN SMALL LETTER E WITH GRAVE
    Character'Val(16#8b#) => Wide_Character'Val(16#00ef#) , -- LATIN SMALL LETTER I WITH DIAERESIS
    Character'Val(16#8c#) => Wide_Character'Val(16#00ee#) , -- LATIN SMALL LETTER I WITH CIRCUMFLEX
    Character'Val(16#8d#) => Wide_Character'Val(16#00ec#) , -- LATIN SMALL LETTER I WITH GRAVE
    Character'Val(16#8e#) => Wide_Character'Val(16#00c4#) , -- LATIN CAPITAL LETTER A WITH DIAERESIS
    Character'Val(16#8f#) => Wide_Character'Val(16#00c5#) , -- LATIN CAPITAL LETTER A WITH RING ABOVE
    Character'Val(16#90#) => Wide_Character'Val(16#00c9#) , -- LATIN CAPITAL LETTER E WITH ACUTE
    Character'Val(16#91#) => Wide_Character'Val(16#00e6#) , -- LATIN SMALL LIGATURE AE
    Character'Val(16#92#) => Wide_Character'Val(16#00c6#) , -- LATIN CAPITAL LIGATURE AE
    Character'Val(16#93#) => Wide_Character'Val(16#00f4#) , -- LATIN SMALL LETTER O WITH CIRCUMFLEX
    Character'Val(16#94#) => Wide_Character'Val(16#00f6#) , -- LATIN SMALL LETTER O WITH DIAERESIS
    Character'Val(16#95#) => Wide_Character'Val(16#00f2#) , -- LATIN SMALL LETTER O WITH GRAVE
    Character'Val(16#96#) => Wide_Character'Val(16#00fb#) , -- LATIN SMALL LETTER U WITH CIRCUMFLEX
    Character'Val(16#97#) => Wide_Character'Val(16#00f9#) , -- LATIN SMALL LETTER U WITH GRAVE
    Character'Val(16#98#) => Wide_Character'Val(16#00ff#) , -- LATIN SMALL LETTER Y WITH DIAERESIS
    Character'Val(16#99#) => Wide_Character'Val(16#00d6#) , -- LATIN CAPITAL LETTER O WITH DIAERESIS
    Character'Val(16#9a#) => Wide_Character'Val(16#00dc#) , -- LATIN CAPITAL LETTER U WITH DIAERESIS
    Character'Val(16#9b#) => Wide_Character'Val(16#00a2#) , -- CENT SIGN
    Character'Val(16#9c#) => Wide_Character'Val(16#00a3#) , -- POUND SIGN
    Character'Val(16#9d#) => Wide_Character'Val(16#00a5#) , -- YEN SIGN
    Character'Val(16#9e#) => Wide_Character'Val(16#20a7#) , -- PESETA SIGN
    Character'Val(16#9f#) => Wide_Character'Val(16#0192#) , -- LATIN SMALL LETTER F WITH HOOK
    Character'Val(16#a0#) => Wide_Character'Val(16#00e1#) , -- LATIN SMALL LETTER A WITH ACUTE
    Character'Val(16#a1#) => Wide_Character'Val(16#00ed#) , -- LATIN SMALL LETTER I WITH ACUTE
    Character'Val(16#a2#) => Wide_Character'Val(16#00f3#) , -- LATIN SMALL LETTER O WITH ACUTE
    Character'Val(16#a3#) => Wide_Character'Val(16#00fa#) , -- LATIN SMALL LETTER U WITH ACUTE
    Character'Val(16#a4#) => Wide_Character'Val(16#00f1#) , -- LATIN SMALL LETTER N WITH TILDE
    Character'Val(16#a5#) => Wide_Character'Val(16#00d1#) , -- LATIN CAPITAL LETTER N WITH TILDE
    Character'Val(16#a6#) => Wide_Character'Val(16#00aa#) , -- FEMININE ORDINAL INDICATOR
    Character'Val(16#a7#) => Wide_Character'Val(16#00ba#) , -- MASCULINE ORDINAL INDICATOR
    Character'Val(16#a8#) => Wide_Character'Val(16#00bf#) , -- INVERTED QUESTION MARK
    Character'Val(16#a9#) => Wide_Character'Val(16#2310#) , -- REVERSED NOT SIGN
    Character'Val(16#aa#) => Wide_Character'Val(16#00ac#) , -- NOT SIGN
    Character'Val(16#ab#) => Wide_Character'Val(16#00bd#) , -- VULGAR FRACTION ONE HALF
    Character'Val(16#ac#) => Wide_Character'Val(16#00bc#) , -- VULGAR FRACTION ONE QUARTER
    Character'Val(16#ad#) => Wide_Character'Val(16#00a1#) , -- INVERTED EXCLAMATION MARK
    Character'Val(16#ae#) => Wide_Character'Val(16#00ab#) , -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    Character'Val(16#af#) => Wide_Character'Val(16#00bb#) , -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    Character'Val(16#b0#) => Wide_Character'Val(16#2591#) , -- LIGHT SHADE
    Character'Val(16#b1#) => Wide_Character'Val(16#2592#) , -- MEDIUM SHADE
    Character'Val(16#b2#) => Wide_Character'Val(16#2593#) , -- DARK SHADE
    Character'Val(16#b3#) => Wide_Character'Val(16#2502#) , -- BOX DRAWINGS LIGHT VERTICAL
    Character'Val(16#b4#) => Wide_Character'Val(16#2524#) , -- BOX DRAWINGS LIGHT VERTICAL AND LEFT
    Character'Val(16#b5#) => Wide_Character'Val(16#2561#) , -- BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    Character'Val(16#b6#) => Wide_Character'Val(16#2562#) , -- BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    Character'Val(16#b7#) => Wide_Character'Val(16#2556#) , -- BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    Character'Val(16#b8#) => Wide_Character'Val(16#2555#) , -- BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    Character'Val(16#b9#) => Wide_Character'Val(16#2563#) , -- BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    Character'Val(16#ba#) => Wide_Character'Val(16#2551#) , -- BOX DRAWINGS DOUBLE VERTICAL
    Character'Val(16#bb#) => Wide_Character'Val(16#2557#) , -- BOX DRAWINGS DOUBLE DOWN AND LEFT
    Character'Val(16#bc#) => Wide_Character'Val(16#255d#) , -- BOX DRAWINGS DOUBLE UP AND LEFT
    Character'Val(16#bd#) => Wide_Character'Val(16#255c#) , -- BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    Character'Val(16#be#) => Wide_Character'Val(16#255b#) , -- BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    Character'Val(16#bf#) => Wide_Character'Val(16#2510#) , -- BOX DRAWINGS LIGHT DOWN AND LEFT
    Character'Val(16#c0#) => Wide_Character'Val(16#2514#) , -- BOX DRAWINGS LIGHT UP AND RIGHT
    Character'Val(16#c1#) => Wide_Character'Val(16#2534#) , -- BOX DRAWINGS LIGHT UP AND HORIZONTAL
    Character'Val(16#c2#) => Wide_Character'Val(16#252c#) , -- BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    Character'Val(16#c3#) => Wide_Character'Val(16#251c#) , -- BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    Character'Val(16#c4#) => Wide_Character'Val(16#2500#) , -- BOX DRAWINGS LIGHT HORIZONTAL
    Character'Val(16#c5#) => Wide_Character'Val(16#253c#) , -- BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    Character'Val(16#c6#) => Wide_Character'Val(16#255e#) , -- BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    Character'Val(16#c7#) => Wide_Character'Val(16#255f#) , -- BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    Character'Val(16#c8#) => Wide_Character'Val(16#255a#) , -- BOX DRAWINGS DOUBLE UP AND RIGHT
    Character'Val(16#c9#) => Wide_Character'Val(16#2554#) , -- BOX DRAWINGS DOUBLE DOWN AND RIGHT
    Character'Val(16#ca#) => Wide_Character'Val(16#2569#) , -- BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    Character'Val(16#cb#) => Wide_Character'Val(16#2566#) , -- BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    Character'Val(16#cc#) => Wide_Character'Val(16#2560#) , -- BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    Character'Val(16#cd#) => Wide_Character'Val(16#2550#) , -- BOX DRAWINGS DOUBLE HORIZONTAL
    Character'Val(16#ce#) => Wide_Character'Val(16#256c#) , -- BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    Character'Val(16#cf#) => Wide_Character'Val(16#2567#) , -- BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    Character'Val(16#d0#) => Wide_Character'Val(16#2568#) , -- BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    Character'Val(16#d1#) => Wide_Character'Val(16#2564#) , -- BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    Character'Val(16#d2#) => Wide_Character'Val(16#2565#) , -- BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    Character'Val(16#d3#) => Wide_Character'Val(16#2559#) , -- BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    Character'Val(16#d4#) => Wide_Character'Val(16#2558#) , -- BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    Character'Val(16#d5#) => Wide_Character'Val(16#2552#) , -- BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    Character'Val(16#d6#) => Wide_Character'Val(16#2553#) , -- BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    Character'Val(16#d7#) => Wide_Character'Val(16#256b#) , -- BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    Character'Val(16#d8#) => Wide_Character'Val(16#256a#) , -- BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    Character'Val(16#d9#) => Wide_Character'Val(16#2518#) , -- BOX DRAWINGS LIGHT UP AND LEFT
    Character'Val(16#da#) => Wide_Character'Val(16#250c#) , -- BOX DRAWINGS LIGHT DOWN AND RIGHT
    Character'Val(16#db#) => Wide_Character'Val(16#2588#) , -- FULL BLOCK
    Character'Val(16#dc#) => Wide_Character'Val(16#2584#) , -- LOWER HALF BLOCK
    Character'Val(16#dd#) => Wide_Character'Val(16#258c#) , -- LEFT HALF BLOCK
    Character'Val(16#de#) => Wide_Character'Val(16#2590#) , -- RIGHT HALF BLOCK
    Character'Val(16#df#) => Wide_Character'Val(16#2580#) , -- UPPER HALF BLOCK
    Character'Val(16#e0#) => Wide_Character'Val(16#03b1#) , -- GREEK SMALL LETTER ALPHA
    Character'Val(16#e1#) => Wide_Character'Val(16#00df#) , -- LATIN SMALL LETTER SHARP S
    Character'Val(16#e2#) => Wide_Character'Val(16#0393#) , -- GREEK CAPITAL LETTER GAMMA
    Character'Val(16#e3#) => Wide_Character'Val(16#03c0#) , -- GREEK SMALL LETTER PI
    Character'Val(16#e4#) => Wide_Character'Val(16#03a3#) , -- GREEK CAPITAL LETTER SIGMA
    Character'Val(16#e5#) => Wide_Character'Val(16#03c3#) , -- GREEK SMALL LETTER SIGMA
    Character'Val(16#e6#) => Wide_Character'Val(16#00b5#) , -- MICRO SIGN
    Character'Val(16#e7#) => Wide_Character'Val(16#03c4#) , -- GREEK SMALL LETTER TAU
    Character'Val(16#e8#) => Wide_Character'Val(16#03a6#) , -- GREEK CAPITAL LETTER PHI
    Character'Val(16#e9#) => Wide_Character'Val(16#0398#) , -- GREEK CAPITAL LETTER THETA
    Character'Val(16#ea#) => Wide_Character'Val(16#03a9#) , -- GREEK CAPITAL LETTER OMEGA
    Character'Val(16#eb#) => Wide_Character'Val(16#03b4#) , -- GREEK SMALL LETTER DELTA
    Character'Val(16#ec#) => Wide_Character'Val(16#221e#) , -- INFINITY
    Character'Val(16#ed#) => Wide_Character'Val(16#03c6#) , -- GREEK SMALL LETTER PHI
    Character'Val(16#ee#) => Wide_Character'Val(16#03b5#) , -- GREEK SMALL LETTER EPSILON
    Character'Val(16#ef#) => Wide_Character'Val(16#2229#) , -- INTERSECTION
    Character'Val(16#f0#) => Wide_Character'Val(16#2261#) , -- IDENTICAL TO
    Character'Val(16#f1#) => Wide_Character'Val(16#00b1#) , -- PLUS-MINUS SIGN
    Character'Val(16#f2#) => Wide_Character'Val(16#2265#) , -- GREATER-THAN OR EQUAL TO
    Character'Val(16#f3#) => Wide_Character'Val(16#2264#) , -- LESS-THAN OR EQUAL TO
    Character'Val(16#f4#) => Wide_Character'Val(16#2320#) , -- TOP HALF INTEGRAL
    Character'Val(16#f5#) => Wide_Character'Val(16#2321#) , -- BOTTOM HALF INTEGRAL
    Character'Val(16#f6#) => Wide_Character'Val(16#00f7#) , -- DIVISION SIGN
    Character'Val(16#f7#) => Wide_Character'Val(16#2248#) , -- ALMOST EQUAL TO
    Character'Val(16#f8#) => Wide_Character'Val(16#00b0#) , -- DEGREE SIGN
    Character'Val(16#f9#) => Wide_Character'Val(16#2219#) , -- BULLET OPERATOR
    Character'Val(16#fa#) => Wide_Character'Val(16#00b7#) , -- MIDDLE DOT
    Character'Val(16#fb#) => Wide_Character'Val(16#221a#) , -- SQUARE ROOT
    Character'Val(16#fc#) => Wide_Character'Val(16#207f#) , -- SUPERSCRIPT LATIN SMALL LETTER N
    Character'Val(16#fd#) => Wide_Character'Val(16#00b2#) , -- SUPERSCRIPT TWO
    Character'Val(16#fe#) => Wide_Character'Val(16#25a0#) , -- BLACK SQUARE
    Character'Val(16#ff#) => Wide_Character'Val(16#00a0#)   -- NO-BREAK SPACE
  );

  function To_UTF_16(s: String; encoding: Zip_name_encoding) return Wide_String
  is
  begin
    case encoding is
      when IBM_437 =>
        declare
          ws: Wide_String(s'Range);
        begin
          for i in s'Range loop
            ws(i):= IBM_437_to_UTF_16(s(i));
          end loop;
          return ws;
        end;
      when UTF_8 =>
        return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
    end case;
  end To_UTF_16;

  function To_UTF_8(s: UTF_16_String) return UTF_8_String is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
  end To_UTF_8;

  function To_UTF_8(s: String; encoding: Zip_name_encoding) return UTF_8_String is
  begin
    case encoding is
      when UTF_8 =>
        return s; -- nothing to do :-)
      when IBM_437 =>
        return To_UTF_8(To_UTF_16(s, encoding));
    end case;
  end To_UTF_8;

  function To_IBM_437(s: UTF_16_String) return String is
    res: String(s'Range);
    found: Boolean;
  begin
    for i in s'Range loop
      found:= False;
      for c in Character loop
        if IBM_437_to_UTF_16(c) = s(i) then
          res(i):= c;
          found:= True;
          exit;
        end if;
      end loop;
      if not found then
        raise Cannot_encode_to_IBM_437;
      end if;
    end loop;
    return res;
  end To_IBM_437;

  function Image(topic: Entry_topic) return UTF_16_String is
    u: constant UTF_16_String:= Entry_topic'Wide_Image(topic);
    l: constant UTF_16_String:= To_Lower(u);
  begin
    case topic is
      when FType =>
        return "Type";
      when CRC32 =>
        return "CRC 32";
      when others =>
        return u(u'First) & l(l'First+1..l'Last);
    end case;
  end Image;

  function Hexadecimal(x: Interfaces.Unsigned_32) return UTF_16_String
  is
    package MIO is new Ada.Wide_Text_IO.Modular_IO(Interfaces.Unsigned_32);
    str: UTF_16_String(1..12);
  begin
    MIO.Put(str, x, 16);
    return str(Index(str,"#")+1..11);
  end Hexadecimal;

  function File_size_image(x: Zip.File_size_type) return UTF_16_String is
    use type Zip.File_size_type;
  begin
    if x >= 1024 ** 3 then
      return File_size_image(x / (1024 ** 3)) & " GB";
    elsif x >= 1024 ** 2 then
      return File_size_image(x / (1024 ** 2)) & " MB";
    elsif x >= 1024 then
      return File_size_image(x / 1024) & " KB";
    else
      return Trim(Zip.File_size_type'Wide_Image(x), Left);
    end if;
  end File_size_image;

  function File_Size_Value(s: UTF_16_String) return Zip.File_size_type is
    use type File_size_type;
  begin
    if Index(s, "KB") > 0 then
      return File_size_type'Wide_Value(s(s'First..s'Last-3)) * 1024;
    elsif Index(s, "MB") > 0 then
      return File_size_type'Wide_Value(s(s'First..s'Last-3)) * 1024**2;
    elsif Index(s, "GB") > 0 then
      return File_size_type'Wide_Value(s(s'First..s'Last-3)) * 1024**3;
    else
      return File_size_type'Wide_Value(s);
    end if;
  end File_Size_Value;

  function Ratio_pct_Image(n,d: Zip.File_size_type) return UTF_16_String is
    use type Zip.File_size_type;
  begin
    if d = 0 then
      return "--";
    else
      return Trim(Integer'Wide_Image(Integer(100.0 * Float(n) / Float(d))), Left) & '%';
    end if;
  end Ratio_pct_Image;

  function Pct_Value(s: UTF_16_String) return Natural is -- 0..101
  begin
    if s = "--" then
      return 101;
    else
      return Integer'Wide_Value(s(s'First..s'Last-1));
    end if;
  end;

  function Is_valid_Zip_archive(file_name: String) return Boolean is
    info: Zip.Zip_info;
  begin
    Zip.Load(info, file_name);
    Zip.Delete(info);
    return True;
  exception
    when others =>
      return False;
  end Is_valid_Zip_archive;

begin
  Zip.Form_For_IO_Open_N_Create:= To_Unbounded_String("encoding=utf8");
end AZip_Common;
