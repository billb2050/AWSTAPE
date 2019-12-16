#include once "file.bi"     ' Change by WHB [needed for fileExists]

DEFINT A-Z
DECLARE SUB EBCtoASC (varString$)
DECLARE SUB ParseCOMMAND ()
DECLARE SUB ProcessDataRecord (buffer$)
DECLARE SUB ProcessHeader1 (buffer$)
DECLARE SUB ProcessHeader2 (buffer$)
DECLARE SUB ProcessTrailer1 (buffer$)
DECLARE SUB ReadDataBlock (buffer$)
DECLARE SUB ReadHeader ()
DECLARE SUB ShowSyntax ()
DECLARE SUB SplitCOMMAND (commandLine$, NumArgs%, Args$())
DECLARE FUNCTION ShowFlags$ ()

DIM SHARED toASCII(1 TO 256) AS INTEGER ' EBCDIC->ASCII table
'DIM SHARED inputFile$ AS STRING * 12 ' name of input AWSTAPE        ' Change by WHB
DIM SHARED inputFile$ ' Change by WHB
DIM SHARED printOption AS INTEGER ' summary/detail print
CONST printSummary% = 0
CONST printDetail% = -1
DIM SHARED awsCurrLRECL AS INTEGER ' header current recd length
DIM SHARED awsPrevLRECL AS INTEGER ' header previous recd length
DIM SHARED awsFlags AS INTEGER ' header flag value
DIM SHARED prevFlags AS INTEGER ' previous header flag value
DIM SHARED labelFileName AS STRING * 17 ' MVT dataset name from HDR1
DIM SHARED labelFileNumber AS STRING * 4 ' MVT file number from HDR1
DIM SHARED labelRECFM AS STRING * 1 ' MVT RECFM from HDR2
DIM SHARED labelBLKSIZE AS INTEGER ' MVT BLKSIZE from HDR2
DIM SHARED labelLRECL AS INTEGER ' MVT LRECL from HDR2
DIM SHARED labelBLKCOUNT AS LONG ' MVT block count from EOF1
DIM SHARED auditBLKCOUNT AS LONG ' my block count
DIM firstSwitch AS INTEGER ' first time indicator

' Load EBCDIC -> ASCII translation table
FOR C = 1 TO 256: READ xCode$: toASCII(C) = VAL("&H" + xCode$): NEXT C

PRINT "TAPEDUMP v1.0 (BASIC) copyright Jay Moseley, CCP 2000"

' This program is placed in the public domain and may be freely used
' and incorporated into derived works so long as attribution to the
' original authorship remains in any distributed copies of the BASIC
' source. /Jay Moseley/ January, 2008
'
' 11/11/2018 Some changes by Bill Blasingim to work with QB64...
'           search for 'WHB' for those changes!
' 10/05/2019 Further changes to allow it to work with FreeBasic.
'            QB64 compiled size was 1.9M as opposed to FreeBasic's 77.9K
'           Change function DIR$ to fileExists
'           Also had to add #include 

'               * * * * I M P O R T A N T * * * *
'               Compile: fbc -ex -lang qb tapedump.bas
ParseCOMMAND

' open AWSTape file input

OPEN inputFile$ FOR BINARY AS #1

PRINT "Processing AWSTAPE file: "; inputFile$

' process AWSTape file
DO
    ReadHeader ' read AWS header block

    IF firstSwitch = 0 THEN
        IF (awsCurrLRECL = 80) AND (awsPrevLRECL = 0) AND (awsFlags = 160) THEN
            firstSwitch = 1
        ELSE
            PRINT "Data for first header block seems to indicate that the file"
            PRINT "specified for input ("; inputFile$; " is not an AWSTAPE dataset."
            PRINT "ABORTING!"
            BEEP
            CLOSE
            END
        END IF
    END IF

    IF printOption = printDetail THEN
        PRINT "Header: ";
        PRINT USING "This: ##,### "; awsCurrLRECL;
        PRINT USING "Prev: ##,### "; awsPrevLRECL;
        PRINT "Flags: "; ShowFlags$
    END IF

    IF awsCurrLRECL > 0 THEN
        ReadDataBlock buffer$
        SELECT CASE LEFT$(buffer$, 4)
            CASE "VOL1"
                IF printOption = printDetail THEN PRINT buffer$
                IF printOption = printSummary THEN
                    PRINT "Serial Number from VOLume 1 label: "; MID$(buffer$, 5, 6)
                    PRINT ""
                    PRINT "File  MVT Dataset       RECFM BLKSIZE  LRECL BlocksExpected ";
                    PRINT "BlocksRead "
                END IF

            CASE "HDR1"
                ProcessHeader1 buffer$
                IF printOption = printDetail THEN PRINT buffer$
                auditBLKCOUNT = 0

            CASE "HDR2"
                ProcessHeader2 buffer$
                IF printOption = printDetail THEN PRINT buffer$

            CASE "EOF1"
                ProcessTrailer1 buffer$
                IF printOption = printDetail THEN PRINT buffer$

            CASE "EOF2"
                IF printOption = printDetail THEN PRINT buffer$
                IF printOption = printSummary THEN
                    PRINT labelFileNumber; "  "; labelFileName; "   ";
                    PRINT labelRECFM; "   ";
                    PRINT USING "###,### "; labelBLKSIZE;
                    PRINT USING "##,### "; labelLRECL;
                    PRINT USING "       ###,### "; labelBLKCOUNT;
                    PRINT USING "   ###,### "; auditBLKCOUNT
                END IF

            CASE ELSE
                ProcessDataRecord buffer$

        END SELECT
    END IF

    IF awsFlags = 64 THEN
        IF printOption = printDetail THEN PRINT "TAPE MARK"
        IF prevFlags = 64 THEN EXIT DO
    END IF
    prevFlags = awsFlags

LOOP

CLOSE

END

' ebcdic_to_ascii
DATA "00","01","02","03","A6","09","A7","7F"
DATA "A9","B0","B1","0B","0C","0D","0E","0F"
DATA "10","11","12","13","B2","B4","08","B7"
DATA "18","19","1A","B8","BA","1D","BB","1F"
DATA "BD","C0","1C","C1","C2","0A","17","1B"
DATA "C3","C4","C5","C6","C7","05","06","07"
DATA "C8","C9","16","CB","CC","1E","CD","04"
DATA "CE","D0","D1","D2","14","15","D3","FC"
DATA "20","D4","83","84","85","A0","D5","86"
DATA "87","A4","D6","2E","3C","28","2B","D7"
DATA "26","82","88","89","8A","A1","8C","8B"
DATA "8D","D8","21","24","2A","29","3B","5E"
DATA "2D","2F","D9","8E","DB","DC","DD","8F"
DATA "80","A5","7C","2C","25","5F","3E","3F"
DATA "DE","90","DF","E0","E2","E3","E4","E5"
DATA "E6","60","3A","23","40","27","3D","22"
DATA "E7","61","62","63","64","65","66","67"
DATA "68","69","AE","AF","E8","E9","EA","EC"
DATA "F0","6A","6B","6C","6D","6E","6F","70"
DATA "71","72","F1","F2","91","F3","92","F4"
DATA "F5","7E","73","74","75","76","77","78"
DATA "79","7A","AD","A8","F6","5B","F7","F8"
DATA "9B","9C","9D","9E","9F","B5","B6","AC"
DATA "AB","B9","AA","B3","BC","5D","BE","BF"
DATA "7B","41","42","43","44","45","46","47"
DATA "48","49","CA","93","94","95","A2","CF"
DATA "7D","4A","4B","4C","4D","4E","4F","50"
DATA "51","52","DA","96","81","97","A3","98"
DATA "5C","E1","53","54","55","56","57","58"
DATA "59","5A","FD","EB","99","ED","EE","EF"
DATA "30","31","32","33","34","35","36","37"
DATA "38","39","FE","FB","9A","F9","FA","FF"

SUB EBCtoASC (varString$)

    FOR C& = 1 TO LEN(varString$)
        ptr1 = ASC(MID$(varString$, C&, 1)) + 1
        MID$(varString$, C&, 1) = CHR$(toASCII(ptr1))
    NEXT C&

END SUB

SUB ParseCOMMAND

    DIM Arg$(1 TO 15)

    commandLine$ = COMMAND$ + " "

    ' If parameters are omitted or any part of parameter line contains
    ' "help" or "?" then print syntax message and terminate

    IF LEN(LTRIM$(RTRIM$(commandLine$))) = 0 THEN
        ShowSyntax
    ELSEIF INSTR(commandLine$, "?") > 0 THEN
        ShowSyntax
    ELSEIF INSTR(commandLine$, "help") > 0 THEN
        ShowSyntax
    END IF

    SplitCOMMAND commandLine$, NumArgs, Arg$()

    'inputFile$ = SPACE$(12)

    FOR ptr1 = 1 TO NumArgs

        SELECT CASE Arg$(ptr1)
            CASE "INPUT", "IN", "I", "INPUT:", "IN:", "I:"
                IF ptr1 = NumArgs THEN
                    PRINT "<input file name> missing following ";
                    PRINT Arg$(ptr1); " parameter"
                    BEEP
                ELSE
                    inputFile$ = Arg$(ptr1 + 1)
                END IF

            CASE "DETAIL", "D"
                printOption = printDetail

        END SELECT

    NEXT ptr1

    IF inputFile$ = "" THEN ' Change by WHB
        PRINT "required INPUT: <file name> parameter omitted"
        ShowSyntax
    ELSE
        Exists% = fileExists(inputFile$) ' Change by WHB
        IF Exists% = 0 THEN ' Change by WHB
            PRINT "specified INPUT: file (";
            PRINT inputFile$; ") does not exist in current subdirectory!"
            BEEP
            END
        END IF
    END IF

END SUB

SUB ProcessDataRecord (buffer$)

    auditBLKCOUNT = auditBLKCOUNT + 1

    IF labelBLKSIZE = labelLRECL THEN
        IF printOption = printDetail THEN PRINT buffer$
        EXIT SUB
    END IF

    offset = 1
    DO
        IF printOption = printDetail THEN PRINT MID$(buffer$, offset, labelLRECL)
        offset = offset + labelLRECL
    LOOP WHILE offset < awsCurrLRECL

END SUB

SUB ProcessHeader1 (buffer$)

    labelFileName = MID$(buffer$, 5, 17)
    labelFileNumber = MID$(buffer$, 32, 4)

END SUB

SUB ProcessHeader2 (buffer$)

    labelRECFM = MID$(buffer$, 39, 1)
    labelBLKSIZE = VAL(MID$(buffer$, 6, 5))
    labelLRECL = VAL(MID$(buffer$, 11, 5))

END SUB

SUB ProcessTrailer1 (buffer$)

    labelBLKCOUNT = VAL(MID$(buffer$, 55, 6))

END SUB

SUB ReadDataBlock (buffer$)

    buffer$ = SPACE$(awsCurrLRECL)
    GET #1, , buffer$
    EBCtoASC buffer$

END SUB

SUB ReadHeader

    header$ = SPACE$(6)
    GET #1, , header$

    awsCurrLRECL = CVI(MID$(header$, 1, 2))
    awsPrevLRECL = CVI(MID$(header$, 3, 2))
    awsFlags = CVI(MID$(header$, 5, 2))

END SUB

FUNCTION ShowFlags$

    bit1$ = "0000"

    work = awsFlags / 16

    bx = 4

    DO
        MID$(bit1$, bx, 1) = RIGHT$(STR$((work MOD 2)), 1)
        bx = bx - 1
        work = work \ 2
    LOOP WHILE work > 0

    flagWord$ = SPACE$(23)

    IF MID$(bit1$, 1, 1) = "1" THEN MID$(flagWord$, 1, 5) = "x'80' "
    IF MID$(bit1$, 2, 1) = "1" THEN MID$(flagWord$, 7, 5) = "x'40' "
    IF MID$(bit1$, 3, 1) = "1" THEN MID$(flagWord$, 13, 5) = "x'20' "
    IF MID$(bit1$, 4, 1) = "1" THEN MID$(flagWord$, 19, 5) = "x'10' "

    ShowFlags = flagWord$

END FUNCTION

SUB ShowSyntax

    PRINT "Syntax: TAPEDUMP  INPUT: | IN: | I: <input file name>"
    PRINT "                  [ DETAIL | D ]"
    BEEP
    END

END SUB

SUB SplitCOMMAND (commandLine$, NumArgs, Args$())
    CONST TRUE = -1, FALSE = 0

    MaxArgs = UBOUND(Args$)
    NumArgs = 0
    In = FALSE

    commandLength = LEN(commandLine$)

    'Go through the command line a character at a time.
    FOR ptr1 = 1 TO commandLength

        C$ = MID$(commandLine$, ptr1, 1)

        'Test for character being a blank or a tab.
        IF (C$ <> " " AND C$ <> CHR$(9)) THEN

            'Neither blank nor tab. Test if you're already inside an argument.
            IF NOT In THEN

                'You've found the start of a new argument.
                'Test for too many arguments.
                IF NumArgs = MaxArgs THEN EXIT FOR

                NumArgs = NumArgs + 1
                In = TRUE
            END IF

            'Add the character to the current argument.
            Args$(NumArgs) = Args$(NumArgs) + C$

        ELSE

            'Found a blank or a tab.
            'Set "Not in an argument" flag to FALSE.
            In = FALSE
        END IF

    NEXT ptr1

END SUB

