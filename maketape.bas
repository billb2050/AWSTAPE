DEFINT A-Z
DECLARE FUNCTION juliandate$ ()
DECLARE FUNCTION Numeric% (arg$)
DECLARE SUB ASCtoEBC (varString$)
DECLARE SUB InsertInOutputBlock (InData$, buffer$)
DECLARE SUB LoadIncludeList (includeFile$)
DECLARE SUB ParseCOMMAND ()
DECLARE SUB ShowSyntax ()
DECLARE SUB SplitCOMMAND (commandLine$, NumArgs%, Args$())
DECLARE SUB WriteBuffer (buffer$)
DECLARE SUB WriteFileLabel (varType$, varSeq%)
DECLARE SUB WriteOutputBlock (buffer$)
DECLARE SUB WriteTapeMark ()
DECLARE SUB WriteVolumeLabel ()
'CONST TRUE = -1, FALSE = 0

DIM SHARED toEBCDIC(1 TO 256) AS INTEGER        ' ASCII->EBCDIC table
DIM SHARED outFile AS STRING * 70               ' output AWSTAPE file
DIM SHARED volumeSerial AS STRING * 6           ' MVT VOL SER in labels
DIM SHARED datasetName AS STRING * 17           ' MVT DSN in labels
DIM SHARED includeFile(1 TO 50) AS STRING * 70  ' input file(s)
DIM SHARED dataLRECL AS INTEGER                 ' input/output LRECL
DIM SHARED dataBLKFACTOR AS INTEGER             ' output blocking factor
DIM SHARED outputGLOBBING AS INTEGER            ' output 1 or many files
CONST outputGLOB% = 0
CONST outputUNIQUE% = -1

DIM SHARED prevLRECL AS INTEGER                 ' AWS header information
DIM SHARED inFileSeq AS INTEGER                 ' input file counter
DIM SHARED blockCount AS LONG                   ' EOF1 block count
DIM SHARED physicalBLKSize AS INTEGER           ' physical block size
DIM SHARED interBLKptr AS INTEGER               ' block construction ptr1

' load ASCII -> EBCDIC translation table
FOR c = 1 TO 256: READ xCode$: toEBCDIC(c) = VAL("&H" + xCode$): NEXT c

PRINT "MAKETAPE v1.2 (BASIC) copyright Jay Moseley, CCP 2000-2002"

' This program is placed in the public domain and may be freely used     
' and incorporated into derived works so long as attribution to the
' original authorship remains in any distributed copies of the BASIC
' source. /Jay Moseley/ January, 2008
'
' 11/5/2018 - Small changes by Bill Blasingim to work with Free Basic compiler
'             on Linux. Commented out TRUE & FALSE constant and apparently
'             Free Basic had a problem with the variable ptr...so renamed 
'             to ptr1. Added the comment to the syntax help...
'             "Note: A space is required AFTER the keyword [or colon]"
' 10/04/2019  Changes for longer input/output file names up to 70 bytes, because
'             I had a input file name with path that was a length of 50
'
'             * * * * I M P O R T A N T * * * *
'             Free Basic compile command: fbc -ex -lang fblite maketape.bas
'
' parse command line
ParseCOMMAND

' if specified output file exists, give option to overwrite/terminate
IF LEN(DIR$(outFile)) > 0 THEN
  PRINT "AWSTAPE File: "; RTRIM$(outFile);
  PRINT " appears to already exist. Do you want to overwrite (Y/N)? ";
  DO: answer$ = UCASE$(INPUT$(1)): LOOP WHILE INSTR("YN", answer$) = 0
  IF answer$ = "N" THEN END
END IF

' open AWSTAPE image file
OPEN outFile FOR BINARY AS #1

' write VOLume 1 label
WriteVolumeLabel

inFileSeq = 1                                   ' input file index
physicalBLKSize = dataLRECL * dataBLKFACTOR     ' compute output BLKSIZE

buffer$ = SPACE$(physicalBLKSize)               ' buffer for AWSTAPE output
interBLKptr = 1                                 ' used for blocking output

' if creating a single output file, write standard HeaDeR labels
IF outputGLOBBING = outputGLOB THEN
  WriteFileLabel "H", 1
  WriteFileLabel "H", 2
  WriteTapeMark
END IF

' process all of the specified input files
DO
  PRINT "Processing input from: "; includeFile(inFileSeq)

  OPEN includeFile(inFileSeq) FOR INPUT AS #2

  ' if writing individual output files, write standard HeaDeR labels
  IF outputGLOBBING = outputUNIQUE THEN
    WriteFileLabel "H", 1
    WriteFileLabel "H", 2
    WriteTapeMark
  END IF

  ' process the contents of the current input file
  WHILE NOT EOF(2)
    LINE INPUT #2, InData$
    InsertInOutputBlock InData$, buffer$
    IF interBLKptr > physicalBLKSize THEN
      WriteOutputBlock buffer$
      buffer$ = SPACE$(physicalBLKSize)
      interBLKptr = 1
    END IF
  WEND

  CLOSE #2                                      ' current input file

  ' if writing individual output files, write standard End Of File labels
  IF outputGLOBBING = outputUNIQUE THEN
    ' if partial block built, write it out
    IF interBLKptr > 1 THEN
      WriteOutputBlock buffer$
      buffer$ = SPACE$(physicalBLKSize)
      interBLKptr = 1
    END IF
    WriteTapeMark
    WriteFileLabel "E", 1
    WriteFileLabel "E", 2
    WriteTapeMark
    blockCount = 0
  END IF

  inFileSeq = inFileSeq + 1

  ' if file limit of 50 input files reached, exit
  IF inFileSeq > 50 THEN EXIT DO

  ' if last specified input file processed, exit
  IF includeFile(inFileSeq) = SPACE$(70) THEN EXIT DO

LOOP

' if writing single output files, write standard End Of File labels
IF outputGLOBBING = outputGLOB THEN
  ' if partial block built, write it out
  IF interBLKptr > 1 THEN
    WriteOutputBlock buffer$
  END IF
  WriteTapeMark
  WriteFileLabel "E", 1
  WriteFileLabel "E", 2
  WriteTapeMark
END IF

WriteTapeMark

CLOSE

END

' ascii_to_ebcdic
        DATA     "00", "01", "02", "03", "37", "2D", "2E", "2F"
        DATA     "16", "05", "25", "0B", "0C", "0D", "0E", "0F"
        DATA     "10", "11", "12", "13", "3C", "3D", "32", "26"
        DATA     "18", "19", "1A", "27", "22", "1D", "35", "1F"
        DATA     "40", "5A", "7F", "7B", "5B", "6C", "50", "7D"
        DATA     "4D", "5D", "5C", "4E", "6B", "60", "4B", "61"
        DATA     "F0", "F1", "F2", "F3", "F4", "F5", "F6", "F7"
        DATA     "F8", "F9", "7A", "5E", "4C", "7E", "6E", "6F"
        DATA     "7C", "C1", "C2", "C3", "C4", "C5", "C6", "C7"
        DATA     "C8", "C9", "D1", "D2", "D3", "D4", "D5", "D6"
        DATA     "D7", "D8", "D9", "E2", "E3", "E4", "E5", "E6"
        DATA     "E7", "E8", "E9", "AD", "E0", "BD", "5F", "6D"
        DATA     "79", "81", "82", "83", "84", "85", "86", "87"
        DATA     "88", "89", "91", "92", "93", "94", "95", "96"
        DATA     "97", "98", "99", "A2", "A3", "A4", "A5", "A6"
        DATA     "A7", "A8", "A9", "C0", "6A", "D0", "A1", "07"
        DATA     "68", "DC", "51", "42", "43", "44", "47", "48"
        DATA     "52", "53", "54", "57", "56", "58", "63", "67"
        DATA     "71", "9C", "9E", "CB", "CC", "CD", "DB", "DD"
        DATA     "DF", "EC", "FC", "B0", "B1", "B2", "B3", "B4"
        DATA     "45", "55", "CE", "DE", "49", "69", "04", "06"
        DATA     "AB", "08", "BA", "B8", "B7", "AA", "8A", "8B"
        DATA     "09", "0A", "14", "BB", "15", "B5", "B6", "17"
        DATA     "1B", "B9", "1C", "1E", "BC", "20", "BE", "BF"
        DATA     "21", "23", "24", "28", "29", "2A", "2B", "2C"
        DATA     "30", "31", "CA", "33", "34", "36", "38", "CF"
        DATA     "39", "3A", "3B", "3E", "41", "46", "4A", "4F"
        DATA     "59", "62", "DA", "64", "65", "66", "70", "72"
        DATA     "73", "E1", "74", "75", "76", "77", "78", "80"
        DATA     "8C", "8D", "8E", "EB", "8F", "ED", "EE", "EF"
        DATA     "90", "9A", "9B", "9D", "9F", "A0", "AC", "AE"
        DATA     "AF", "FD", "FE", "FB", "3F", "EA", "FA", "FF"

SUB ASCtoEBC (varString$)

FOR c& = 1 TO LEN(varString$)
  ptr1 = ASC(MID$(varString$, c&, 1)) + 1
  MID$(varString$, c&, 1) = CHR$(toEBCDIC(ptr1))
NEXT c&

END SUB

SUB InsertInOutputBlock (InData$, buffer$)

MID$(buffer$, interBLKptr, dataLRECL) = LEFT$(InData$, dataLRECL)
interBLKptr = interBLKptr + dataLRECL

END SUB

FUNCTION juliandate$

d$ = DATE$
m = VAL(MID$(d$, 1, 2))
d = VAL(MID$(d$, 4, 2))
y = VAL(MID$(d$, 7, 4))

rem4 = y MOD 4
rem100 = y MOD 100
rem400 = y MOD 400

dayValue = 0

IF m > 1 THEN dayValue = dayValue + 31
IF m > 2 THEN
  IF rem4 = 0 AND (rem100 <> 0 OR rem400 = 0) THEN
    dayValue = dayValue + 29
  ELSE
    dayValue = dayValue + 28
  END IF
END IF
IF m > 3 THEN dayValue = dayValue + 31
IF m > 4 THEN dayValue = dayValue + 30
IF m > 5 THEN dayValue = dayValue + 31
IF m > 6 THEN dayValue = dayValue + 30
IF m > 7 THEN dayValue = dayValue + 31
IF m > 8 THEN dayValue = dayValue + 31
IF m > 9 THEN dayValue = dayValue + 30
IF m > 10 THEN dayValue = dayValue + 31
IF m > 11 THEN dayValue = dayValue + 30

dayValue = dayValue + d

juliandate$ = RIGHT$(d$, 2) + RIGHT$(STR$(1000 + dayValue), 3)

END FUNCTION

SUB LoadIncludeList (metaFile$)

IF LEN(DIR$(metaFile$)) = 0 THEN
  PRINT "INPUT: @"; metaFile$;
  PRINT " - file specified is not present in current subdirectory!"
  BEEP
  EXIT SUB
END IF

OPEN metaFile$ FOR INPUT AS #1
r = 0: f = 0

DO
  r = r + 1
  INPUT #1, singleFile$
  IF LEN(DIR$(singleFile$)) = 0 THEN
    PRINT "included file #"; LTRIM$(STR$(r)); " - "; singleFile$;
    PRINT " - file specified is not present in current subdirectory!"
    BEEP
  ELSE
    f = f + 1
    IF f > 50 THEN
      PRINT "number of files specified in "; RTRIM$(metaFile$); " exceeds 50"
      PRINT "excess files specified ignored!"
      BEEP
      EXIT DO
    END IF
    includeFile(f) = singleFile$
  END IF
LOOP WHILE NOT EOF(1)

CLOSE #1

END SUB

FUNCTION Numeric (arg$)

rc = 0

FOR ptr1 = 1 TO LEN(arg$)
  SELECT CASE MID$(arg$, ptr1, 1)
    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
      ' valid numeric
    CASE ELSE
      rc = 1
      EXIT FOR
  END SELECT
NEXT ptr1

IF rc = 0 THEN
  Numeric = TRUE
ELSE
  Numeric = FALSE
END IF
      
END FUNCTION

SUB ParseCOMMAND

DIM arg$(1 TO 15)

commandLine$ = COMMAND$

IF LEN(LTRIM$(RTRIM$(commandLine$))) = 0 THEN
  ShowSyntax
ELSEIF INSTR(commandLine$, "?") > 0 THEN
  ShowSyntax
ELSEIF INSTR(commandLine$, "help") > 0 THEN
  ShowSyntax
END IF

' initialize parameter defaults
outFile = SPACE$(70)
volumeSerial = SPACE$(6)
datasetName = SPACE$(17)
FOR i = 1 TO 50: includeFile(i) = SPACE$(70): NEXT i
dataLRECL = 80
dataBLKFACTOR = 1
outputGLOBBING = outputGLOB

SplitCOMMAND commandLine$, NumArgs, arg$()

FOR ptr1 = 1 TO NumArgs

  SELECT CASE arg$(ptr1)
    CASE "INPUT", "IN", "I", "INPUT:", "IN:", "I:"
      IF ptr1 = NumArgs THEN
        PRINT "<input file name> missing following ";
        PRINT arg$(ptr1); " keyword"
        BEEP
      ELSE
        IF LEFT$(arg$(ptr1 + 1), 1) = "@" THEN
          LoadIncludeList MID$(arg$(ptr1 + 1), 2)
        ELSE
          IF LEN(DIR$(arg$(ptr1 + 1))) = 0 THEN
            PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
            PRINT " - file specified is not present in current subdirectory!"
            BEEP
          ELSE
            includeFile(1) = arg$(ptr1 + 1)
          END IF
        END IF
      END IF

    CASE "OUTPUT", "OUT", "O", "OUTPUT:", "OUT:", "O:"
      IF ptr1 = NumArgs THEN
        PRINT "<output file name> missing following ";
        PRINT arg$(ptr1); " keyword"
        BEEP
      ELSE
        outFile = arg$(ptr1 + 1)
      END IF

    CASE "VOLSER", "VOL", "V", "VOLSER:", "VOL:", "V:"
      IF ptr1 = NumArgs THEN
        PRINT "<serial number> missing following ";
        PRINT arg$(ptr1); " keyword"
        BEEP
      ELSE
        RSET volumeSerial = arg$(ptr1 + 1)
      END IF

    CASE "DATASET", "DATA", "D", "DATASET:", "DATA:", "D:"
      IF ptr1 = NumArgs THEN
        PRINT "<dataset name> missing following ";
        PRINT arg$(ptr1); " parameter"
        BEEP
      ELSE
        LSET datasetName = arg$(ptr1 + 1)
      END IF

    CASE "LRECL", "L", "LRECL:", "L:"
      IF ptr1 = NumArgs THEN
        PRINT "<record length> missing following ";
        PRINT arg$(ptr1); " keyword"
        BEEP
      ELSE
        IF NOT Numeric(arg$(ptr1 + 1)) THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value is not numeric"
          BEEP
        ELSEIF VAL(arg$(ptr1 + 1)) = 0 THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value cannot be zero"
          BEEP
        ELSEIF VAL(arg$(ptr1 + 1)) > 200 THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value cannot exceed 200"
          BEEP
        ELSE
          dataLRECL = VAL(arg$(ptr1 + 1))
        END IF
      END IF

    CASE "BLOCK", "B", "BLOCK:", "B:"
      IF ptr1 = NumArgs THEN
        PRINT "<blocking factor> missing following ";
        PRINT arg$(ptr1); " keyword"
        BEEP
      ELSE
        IF NOT Numeric(arg$(ptr1 + 1)) THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value is not numeric"
          BEEP
        ELSEIF VAL(arg$(ptr1 + 1)) = 0 THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value cannot be zero"
          BEEP
        ELSEIF VAL(arg$(ptr1 + 1)) > 255 THEN
          PRINT arg$(ptr1); " "; arg$(ptr1 + 1);
          PRINT " - parameter value cannot exceed 255"
          BEEP
        ELSE
          dataBLKFACTOR = VAL(arg$(ptr1 + 1))
        END IF
      END IF

    CASE "UNIQUE", "U"
      outputGLOBBING = outputUNIQUE

  END SELECT

NEXT ptr1

TermFlag = 0
IF outFile = SPACE$(70) THEN
  PRINT "required OUTPUT: <output file name> omitted"
  TermFlag = TermFlag + 1
END IF
IF volumeSerial = SPACE$(6) THEN
  PRINT "required VOLSER: <volume serial number> omitted"
  TermFlag = TermFlag + 1
END IF
IF datasetName = SPACE$(17) THEN
  PRINT "required DATASET: <dataset name> omitted"
  TermFlag = TermFlag + 1
END IF
IF includeFile(1) = SPACE$(70) THEN
  PRINT "required INPUT: <input file name> omitted or not found"
  TermFlag = TermFlag + 1
END IF

IF TermFlag > 0 THEN ShowSyntax

END SUB

SUB ShowSyntax

PRINT "Syntax: MAKETAPE "
PRINT "           INPUT: <input file name> | @<file containing file list>"
PRINT "           VOLSER: <1 to 6 character volume serial number>"
PRINT "           DATASET: <1 to 17 character dataset label>"
PRINT "           OUTPUT: <output file name>"
PRINT "           [ LRECL: <record length> ]"
PRINT "           [ BLOCK: <blocking factor> ]"
PRINT "           [ UNIQUE ]"
PRINT
PRINT " Note: A space is required AFTER the keyword [or colon]"
PRINT
BEEP
END

END SUB

SUB SplitCOMMAND (commandLine$, NumArgs, Args$())

MaxArgs = UBOUND(Args$)
NumArgs = 0
In = FALSE

commandLength = LEN(commandLine$)

'Go through the command line a character at a time.
FOR ptr1 = 1 TO commandLength

  c$ = MID$(commandLine$, ptr1, 1)

  'Test for character being a blank or a tab.
  IF (c$ <> " " AND c$ <> CHR$(9)) THEN

    'Neither blank nor tab. Test if you're already inside an argument.
    IF NOT In THEN

      'You've found the start of a new argument.
      'Test for too many arguments.
      IF NumArgs = MaxArgs THEN EXIT FOR

      NumArgs = NumArgs + 1
      In = TRUE
    END IF

    'Add the character to the current argument.
    Args$(NumArgs) = Args$(NumArgs) + c$

  ELSE

    'Found a blank or a tab.
    'Set "Not in an argument" flag to FALSE.
    In = FALSE
  END IF

NEXT ptr1

END SUB

SUB WriteBuffer (buffer$)

header$ = SPACE$(6)
MID$(header$, 1, 2) = MKI$(LEN(buffer$))
MID$(header$, 3, 2) = MKI$(prevLRECL)
MID$(header$, 5, 2) = MKI$(160)

PUT #1, , header$

ASCtoEBC buffer$

PUT #1, , buffer$

prevLRECL = LEN(buffer$)

END SUB

SUB WriteFileLabel (varType$, varSeq)

IF varType$ = "H" THEN
  LabelType$ = "HDR"
ELSE
  LabelType$ = "EOF"
END IF

labelBuffer$ = SPACE$(80)

TdatasetName$ = RTRIM$(datasetName)
TfileSeq$ = RIGHT$(STR$(10000 + inFileSeq), 4)
IF outputGLOBBING = outputUNIQUE THEN
  IF LEN(TdatasetName$) > 11 THEN
    TdatasetName$ = LEFT$(TdatasetName$, 12) + ".F" + TfileSeq$
  ELSE
    TdatasetName$ = TdatasetName$ + ".F" + TfileSeq$
  END IF
END IF
TdatasetName$ = LEFT$(TdatasetName$ + SPACE$(17), 17)

SELECT CASE varSeq
  CASE 1
    MID$(labelBuffer$, 1, 4) = LabelType$ + RIGHT$(STR$(varSeq), 1)
    MID$(labelBuffer$, 5, 17) = TdatasetName$
    MID$(labelBuffer$, 22, 6) = volumeSerial
    MID$(labelBuffer$, 28, 4) = "0001"          ' volume sequence number
    IF outputGLOBBING = outputUNIQUE THEN
      MID$(labelBuffer$, 32, 4) = TfileSeq$     ' file number
    ELSE
      MID$(labelBuffer$, 32, 4) = "0001"        ' file number
    END IF
    MID$(labelBuffer$, 36, 4) = "    "          ' generation number
    MID$(labelBuffer$, 40, 2) = "  "            ' version number
    MID$(labelBuffer$, 42, 6) = " " + juliandate$       ' creation date
    MID$(labelBuffer$, 48, 6) = " 99365"        ' expiration date
    MID$(labelBuffer$, 54, 1) = "0"             ' security - 0=none
    IF varType$ = "H" THEN
      MID$(labelBuffer$, 55, 6) = "000000"      ' block count
    ELSE
      MID$(labelBuffer$, 55, 6) = RIGHT$(STR$(1000000 + blockCount&), 6)
    END IF
    MID$(labelBuffer$, 61, 20) = SPACE$(20)     ' unused

  CASE 2
    MID$(labelBuffer$, 1, 4) = LabelType$ + RIGHT$(STR$(varSeq), 1)
    MID$(labelBuffer$, 5, 1) = "F"              ' record format
    TblockSize$ = RIGHT$(STR$(100000 + (dataLRECL * dataBLKFACTOR)), 5)
    MID$(labelBuffer$, 6, 5) = TblockSize$      ' block size
    Tlrecl$ = RIGHT$(STR$(100000 + dataLRECL), 5)
    MID$(labelBuffer$, 11, 5) = Tlrecl$         ' record length
    MID$(labelBuffer$, 16, 1) = "3"             ' density
    MID$(labelBuffer$, 17, 1) = "0"             ' dataset position
    MID$(labelBuffer$, 18, 17) = "EXTERNAL/EXTERNAL  ' job name/job step"
    MID$(labelBuffer$, 35, 2) = "  "            ' TRTCH (7-track only)
    MID$(labelBuffer$, 37, 1) = " "             ' control character
    MID$(labelBuffer$, 38, 1) = " "             ' reserved
    IF dataBLKFACTOR = 1 THEN
      Trecfm$ = " "
    ELSE
      Trecfm$ = "B"
    END IF
    MID$(labelBuffer$, 39, 1) = Trecfm$         ' block attributed - b=unblocked
    MID$(labelBuffer$, 40, 8) = SPACE$(8)       ' reserved
    MID$(labelBuffer$, 48, 1) = " "             ' C=checkpoint dataset
    MID$(labelBuffer$, 49, 32) = SPACE$(32)     ' unused

END SELECT

WriteBuffer labelBuffer$

IF varType$ = "E" AND varSeq = 2 THEN
  PRINT "Wrote"; blockCount; " blocks to AWSTAPE file: ";
  PRINT RTRIM$(outFile); " (File #";
  IF outputGLOBBING = outputUNIQUE THEN
    PRINT TfileSeq$; " Dataset: "; RTRIM$(TdatasetName$);
  ELSE
    PRINT "0001"; " Dataset: "; RTRIM$(datasetName$);
  END IF
  PRINT ")"
END IF

END SUB

SUB WriteOutputBlock (buffer$)

IF interBLKptr < physicalBLKSize THEN
  shortBuffer$ = SPACE$(interBLKptr - 1)
  shortBuffer$ = LEFT$(buffer$, interBLKptr - 1)
  WriteBuffer shortBuffer$
ELSE
  WriteBuffer buffer$
END IF

blockCount = blockCount + 1

END SUB

SUB WriteTapeMark

header$ = SPACE$(6)
MID$(header$, 1, 2) = MKI$(0)
MID$(header$, 3, 2) = MKI$(prevLRECL)
MID$(header$, 5, 2) = MKI$(64)

PUT #1, , header$

prevLRECL = 0

END SUB

SUB WriteVolumeLabel

volBuffer$ = SPACE$(80)

MID$(volBuffer$, 1, 4) = "VOL1"
MID$(volBuffer$, 5, 6) = volumeSerial
MID$(volBuffer$, 11, 1) = "0"

WriteBuffer volBuffer$

END SUB

