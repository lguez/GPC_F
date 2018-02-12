MODULE GEMPRM
  ! global declarations
  !use, intrinsic :: ISO_C_BINDING

  ! --------------------------------------------------

  !  Missing data definitions 

  REAL :: 	RMISSD	=  -9999.0 	! Missing data value		 
  REAL :: 	RDIFFD	=  0.1 	    ! Missing value fuzziness	 
  INTEGER ::	IMISSD	=  -9999 	! Missing integer value	 
  INTEGER ::  G_NORMAL	= (   0 )

  !---------------------------------------------------------------------
  INTEGER ::	MAXPATHLEN = 1024
  !  Physical and mathematical constants 

  REAL :: 	PI			=  3.14159265 
  REAL :: 	M_PI		=  3.14159265358979323846
  REAL :: 	HALFPI		=  3.14159265 / 2.0 
  REAL :: 	TWOPI		=  2.0 * 3.14159265 
  REAL :: 	PI4TH		=  3.14159265 / 4.0 
  REAL :: 	PI3RD		=  3.14159265 / 3.0 	! PI,...			
  REAL :: 	DTR			=  3.14159265 / 180.0 
  REAL :: 	RTD			=  180.0 / 3.14159265 	! Degrees <--> Radians		
  REAL :: 	S2HR		=  3600.0 
  REAL :: 	HR2S		=  2.778e-4 	! Seconds <--> Hours		
  REAL :: 	SM2M		=  1609.34 
  REAL :: 	M2SM		=  6.21e-4 	    ! Statute miles <--> Meters	
  REAL :: 	MS2SMH		=  1609.34 * 6.21e-4
  REAL :: 	SMH2MS		=  2.778e-4 * 1609.34 	! Meters/Second <--> Miles/Hour
  REAL :: 	NM2M		=  1852.0 
  REAL :: 	M2NM		=  5.4e-4 	    ! Nautical miles <--> Meters	
  REAL :: 	F2M			=  0.3048 
  REAL :: 	M2F			=  3.2808 	    ! Feet  <--> Meters		
  REAL :: 	MS2NMH		=  3600.0 * 5.4e-4 
  REAL :: 	NMH2MS		=  2.778e-4 * 5.4e-4 	! Nautical miles/hour <-> Meters/Sec 
  REAL :: 	RADIUS		=  6371200.0 	! Earth radius			
  REAL :: 	OMEGA		=  7.2921e-5 	! Earth angular veclocity	
  REAL :: 	GRAVTY		=  9.80616 	    ! Acceleration of gravity	
  REAL :: 	RDGAS		=  287.04 
  REAL :: 	RKAP		=  287.04 / 9.80616 	! Gas constant of dry air	
  REAL :: 	RKAPPA		=  2.0 / 7.0 
  REAL :: 	AKAPPA		=  7.0 / 2.0 	! Poisson constant;inverse	
  REAL :: 	GAMUSD		=  6.5 	        ! US std atmos lapse rate	
  REAL :: 	TMCK		=  273.15 	    ! Celsius -> Kelvin	
  REAL(8) ::  DBL_MAX     =  1.7976E30
  INTEGER :: INT_MAX      =  2147483647

  !---------------------------------------------------------------------

  !  File information parameters 

  INTEGER :: MMKEY	=    12 	! Maximum # of keys 
  INTEGER :: MMHDRS	=  30000 + 200  ! Maximum # of headers 
  INTEGER :: MMPRT	=    20 	! Maximum # of parts 
  INTEGER :: MMLIST	=    20 	! Maximum search list 
  INTEGER :: MMFREE	=    62 	! Number of free pairs 
  INTEGER :: MMFILE	=     3 	! Maximum # of open files 
  INTEGER :: MBLKSZ	=   128 	! Block size 
  INTEGER :: MCACHE	=     8 	! # of cached records 
  INTEGER :: MMPARM	=    44 	! Maximum # of parameters 
  INTEGER :: MMFHDR	=    10 	! Maximum # of file hdrs 
  INTEGER :: MMSRCH	=    30 	! Max # of cond searches 
  !INTEGER :: LLPATH  =  1024 	! Largest path name. 
  PARAMETER (LLPATH  =  1024 )	! Largest path name. 
  !  Consciously using 100 rather than MAXNAMLEN  
  INTEGER :: MXFLSZ	=  100 + 1 	! Maximum file name size (not including path) 
  !  PLEASE, do NOT use FILE_NAMESZ; it is being deprecated  
  INTEGER :: FILE_NAMESZ =  101  ! Largest file name 
  INTEGER :: FILE_FULLSZ =  1024 + 101  ! Largest full (path + file) name 
  INTEGER :: MXNMFL	=   2000 	! Maximum number of files per directory       
  INTEGER :: MXTMPL	=  48 + 1 	! Maximum template size (not including path) 
  INTEGER :: MMFLDP	=  3 * 20 

  !---------------------------------------------------------------------

  !  Set the machine type 

  INTEGER ::  MTVAX	=   2 
  INTEGER ::  MTSUN	=   3 
  INTEGER ::  MTIRIS	=   4 
  INTEGER ::  MTAPOL	=   5 
  INTEGER ::  MTIBM	=   6 
  INTEGER ::  MTIGPH	=   7 
  INTEGER ::  MTULTX	=   8 
  INTEGER ::  MTHP	=   9 
  INTEGER ::  MTALPH	=  10 
  INTEGER ::  MTLNUX	=  11 

  !---------------------------------------------------------------------

  !#if defined(IRIX) || defined(OSF1) || defined(ULTRIX)
  !INTEGER ::  MMRECL		=  1 	! Multiplier for RECL in file  
  !#else
  INTEGER ::  MMRECL		=  4 	! create/open (usually 4 on UNIX sys) 
  !#endif

  INTEGER ::  MDREAL		=    1 
  INTEGER ::  MDINTG		=    2 
  INTEGER ::  MDCHAR		=    3 
  INTEGER ::  MDRPCK		=    4 
  INTEGER ::  MDGRID		=    5 	! Data types in DM lib 
  INTEGER ::  MDGNON		=    0 
  INTEGER ::  MDGGRB		=    1 
  INTEGER ::  MDGNMC		=    2 
  INTEGER ::  MDGDIF		=    3 
  INTEGER ::  MDGDEC		=    4 	! Grid packing types 
  INTEGER ::  MFSF		=    1 
  INTEGER ::  MFSN		=    2 
  INTEGER ::  MFGD		=    3 	! Data file types 
  INTEGER ::  MFUNKN		=   99 
  INTEGER ::  MFAIRW		=    1 
  INTEGER ::  MFMETR		=    2 
  INTEGER ::  MFSHIP		=    3 	! Unknown, airways, metar, ship data source 
  INTEGER ::  MFBUOY		=    4 
  INTEGER ::  MFSYNP		=    5 	! Buoy and synoptic data source 
  INTEGER ::  MFRAOB		=    4 
  INTEGER ::  MFVAS		=    5 	! Rawinsonde and VAS data source 
  INTEGER ::  MFGRID		=    6 	! Grid source 
  INTEGER ::  MFCOUN  	=    7 	! Watch-by-county data source 
  INTEGER ::  MFTEXT		=  100 	! Text 

  !---------------------------------------------------------------------

  !  Declarations for array sizes in programs 

  INTEGER :: LLAXIS		=       64 	! Max # of axis labels 
  INTEGER :: LLCLEV		=       50 	! Max # of contour lvls 
  INTEGER :: LLGDHD		=      128 	! Max grid hdr length 
  INTEGER :: LLMDGG		=  4000000 	! Max mem for intern grids 
  INTEGER :: LLMXDT		=  44 * 500  ! Max # data points 
  INTEGER :: LLMXGD		=  1000000 	! Max # grid points 
  INTEGER :: LLMXGT		=     2000 	! Max # grid times 
  INTEGER :: LLMXLN		=  128 + 1 	! Max length of input 
  INTEGER :: LLMXLV		=      500 	! Max # levels/station 
  INTEGER :: LLMXPT		=    80000 	! Max # lines, polygons, ... 
  INTEGER :: LLMXST		=       20 	! Max # stations in list 
  INTEGER :: LLMXTG		=  1000000 	! Ultimate max # grid points 
  INTEGER :: LLMXTM		=      200 	! Max # times/dataset 
  INTEGER :: LLNANL		=      128 	! Grid anl block length 
  INTEGER :: LLNNAV		=      256 	! Grid nav block length 
  INTEGER :: LLOAGD		=      400 	! Max # grids from 1 OA 
  INTEGER :: LLSTFL		=    30000 	! Max # stations in file 
  INTEGER :: LLSTHL		=       20 	! Max header size 
  INTEGER :: LLTMCX		=      100 	! Max # of time xsect pts 
  INTEGER :: MAX_CNTY		=      400 	! Max # of counties in watch 
  INTEGER :: MAXTMPLT		=       50 	! Maximum data templates 
  INTEGER :: MXLOOP		=       30 	! Maximum frames in loop 

  !---------------------------------------------------------------------

  !  Image file types.  Defined here and in gemprm.OS 

  INTEGER :: IFINVD		=  -1 	! Invalid image file format 
  INTEGER :: IFAREA		=   1 	! MCIDAS AREA file	
  INTEGER :: IFGINI		=   2 	! AWIPS GINI format	
  INTEGER :: IFNIDS		=   3 	! Nexrad NIDS format	
  INTEGER :: IFNOWR		=   4 	! WSI NOWRad format	
  INTEGER :: IFNCDF		=   5 	! NetCDF file		
  INTEGER :: IFNEXZ		=   6 	! NEXRAD zlib compressed 
  INTEGER :: IFNFAX		=   7 	! 6-bit FAX file 
  INTEGER :: IA2DBGINI	=   8 	! A2DB sat GINI image 
  INTEGER :: IA2DBNIDS	=   9 	! A2DB radar NIDS image 
  INTEGER :: IA2DBMOSA	=  10 	! A2DB radar MOSAIC image 
  INTEGER :: IA2DBMCID	=  11 	! A2DB sat MCIDAS image 
  INTEGER :: IFHINIDS		=  12   ! Nexrad Higher Resolution NIDS format   
  !---------------------------------------------------------------------

  !  Data category and subcategory types 

  INTEGER :: CAT_NIL		=  0 	! None - Not useful 
  INTEGER :: CAT_IMG		=  1 	! Images 
  INTEGER :: CAT_SFC		=  2 	! Surface observations 
  INTEGER :: CAT_SFF		=  3 	! Surface forecast = (e.g., MOS, TAF) 
  INTEGER :: CAT_SND		=  4 	! Upper air observations 
  INTEGER :: CAT_SNF		=  5 	! Upper air forecast 
  INTEGER :: CAT_GRD		=  6 	! Grid 
  INTEGER :: CAT_VGF		=  7 	! Vector graphics file 
  INTEGER :: CAT_MSC		=  8 	! Misc = (e.g., Watches, Warnings) 
  INTEGER :: CAT_ENS		=  9 	! Ensembles  

  INTEGER :: SCAT_NIL		=  0 	! None - Not useful 
  INTEGER :: SCAT_SFC		=  1 	! Surface data in daily files 
  INTEGER :: SCAT_SHP		=  2 	! Ship format data in hourly files 
  INTEGER :: SCAT_SFF		=  3 	! Surface forecast = (e.g., MOS, TAF) 
  INTEGER :: SCAT_FFG		=  4 	! Flash flood guidance 
  INTEGER :: SCAT_SND		=  5 	! Upper air data 
  INTEGER :: SCAT_SNF		=  6 	! Upper air forecast 
  INTEGER :: SCAT_FCT		=  7 	! Grid - forecast 
  INTEGER :: SCAT_ANL		=  8 	! Grid - analysis 

  !---------------------------------------------------------------------

  INTEGER :: NO_IMG		=  0 
  INTEGER :: SAT_IMG	=  1 
  INTEGER :: RAD_IMG	=  2 

  !---------------------------------------------------------------------

  !  ASCII character constants 

  CHARACTER (LEN= 1) :: 	CHNULL = CHAR (0)		! Null 
  CHARACTER (LEN= 1) :: 	CHCTLA = CHAR (1)		! Control A - Start of header 
  CHARACTER (LEN= 1) :: 	CHCTLC = CHAR (3)		! Control C - End of text 
  CHARACTER (LEN= 1) :: 	CHTAB  = CHAR (9)		! Tab 
  CHARACTER (LEN= 1) :: 	CHLF   = CHAR (10)		! Line feed 
  CHARACTER (LEN= 1) :: 	CHFF   = CHAR (12)		! Form feed 
  CHARACTER (LEN= 1) :: 	CHCR   = CHAR (13)		! Carriage return 
  CHARACTER (LEN= 1) :: 	CHCAN  = CHAR (24)		! Cancel 
  CHARACTER (LEN= 1) :: 	CHESC  = CHAR (27)		! Escape 
  CHARACTER (LEN= 1) :: 	CHFS   = CHAR (28)		! FS 
  CHARACTER (LEN= 1) :: 	CHGS   = CHAR (29)		! GS 
  CHARACTER (LEN= 1) :: 	CHRS   = CHAR (30)		! Record separator 
  CHARACTER (LEN= 1) :: 	CHUS   = CHAR (31)		! US 
  CHARACTER (LEN= 1) :: 	CHSPAC = CHAR (32)		! Space 
  CHARACTER (LEN= 1) :: 	CHBKSL = CHAR (92)		! Backslash 
  CHARACTER (LEN= 1) :: 	CHTLDA = CHAR (126)		! Tilda 

  !---------------------------------------------------------------------

  !  Other useful constants, specifically for C 

  INTEGER ::  LLBSIZ		=   1024  ! I/O Buffer size 
  INTEGER ::  LLSCRN		=     80  ! Size of screen input/output strings. 
  INTEGER ::  MAXTYP		=    300  ! Maximum number of data types 
  INTEGER ::  MXERST		=    100  ! Maximum # of error messages in buffer   

  INTEGER ::  DTTMSZ		=     21  ! GEMPAK date/time string size 
  INTEGER ::  DTTMS_SIZE	=     12  ! GEMPAK time string size (YYMMDD/HHMM) 
  INTEGER ::  FDTTMS_SIZE	= 12 + 6 ! forcast version (YYMMDD/HHMMfHHHMM) 

  INTEGER ::  NDAYS		=     15 
  INTEGER ::  NFILES		=  15 * 24  ! Maximum number of days saved by LDM,
  !											Number of files for NDAYS 

  INTEGER ::  G_TRUE		=    1  ! TRUE 
  INTEGER ::  G_FALSE		=    0  ! FALSE 
  INTEGER ::  G_RESET		=   -1  ! RESET attributes value 
  INTEGER ::  G_FSRCH		=    0  ! Search forward 
  INTEGER ::  G_BSRCH		=    1  ! Search backward 

  REAL ::	GDIFFD	= 0.000001
  !PARAMETER	( GDIFFD	= 0.000001)


  !---------------------------------------------------------------------

  !  Type definitions */

  CHARACTER (LEN=90) :: 	dattm_t(21)   !	/* date/time type */
  COMMON/DATTM_T_T/dattm_t

  CHARACTER (LEN=90) :: 	dttms_t(12)  !	/* date/time group */
  COMMON/DTTMS_T_T/dttms_t

  CHARACTER (LEN=90) :: 	fdttms_t(18) !	/* forecast version */
  COMMON/FDTTMS_T_T/fdttms_t

  CHARACTER (LEN=90) :: 	nmlst_t(101)   !	/* Type for list of names */
  COMMON/NMLST_T_T/nmlst_t

  !---------------------------------------------------------------------*/

  !
  ! *  Display filter definitions
  ! 
  !INTEGER :: 	MAX_FILTER_NUM		=  50 
  PARAMETER (	MAX_FILTER_NUM		=  50 )
  INTEGER :: 	DSPLY_FILTER_SZ		=  10 
  !typedef char	filter_t[10]
  CHARACTER (LEN=90) :: filter_t(10)
  !---------------------------------------------------------------------*/

  !
  !  Cursor definitions
  !
  INTEGER :: 	CURS_DEFAULT		=  0 
  INTEGER :: 	CURS_POINT_SELECT	=  1 
  INTEGER :: 	CURS_BUSY		    =  2 


  !---------------------------------------------------------------------*/

  !
  !  Special predefined map button index in mapw
  !
  INTEGER ::  EXTRA_PDFBTN	=  2   !  /* extra predefined geog buttons */
  INTEGER ::  SAT_BTN			=  2   !  /* SAT button (next to last button) */
  INTEGER ::  CUSTOM_BTN		=  1   !  /* customize button (=last button) */

  !---------------------------------------------------------------------*/

  ! 
  !  Coordinate system abbreviations 
  !

  CHARACTER (LEN=1) :: sys_S = "S"
  CHARACTER (LEN=1) :: sys_D = "D"
  CHARACTER (LEN=1) :: sys_N = "N"
  CHARACTER (LEN=1) :: sys_V = "V"
  CHARACTER (LEN=1) :: sys_P = "P"
  CHARACTER (LEN=1) :: sys_L = "L"
  CHARACTER (LEN=1) :: sys_U = "U"
  CHARACTER (LEN=1) :: sys_W = "W"
  CHARACTER (LEN=1) :: sys_M = "M"
  CHARACTER (LEN=1) :: sys_Q = "Q"
  CHARACTER (LEN=1) :: sys_I = "I"
  CHARACTER (LEN=1) :: sys_G = "G"

  !---------------------------------------------------------------------*/

  !
  !  Maximum tag name length (based on 80 column table max).
  !
  INTEGER :: 	MXTAGN			=  77 

  !---------------------------------------------------------------------*/

  INTEGER :: 	MAX_LAYERS	=  20 	!/* Layer controls */
  INTEGER :: 	MAXPTS			=  500  !/* Maximum number of points in a line */
  INTEGER :: 	NFILLPAT		=  7 	!/* Number of Fill Patterns in PGEN */

  !---------------------------------------------------------------------*/

  !
  !  Display Depth definitions
  !
  !#define	_8_BIT			( 8 )
  INTEGER :: 	I8_BIT			=  8 

  !---------------------------------------------------------------------*/
  !
  ! Max Uncompressed block (in bytes)
  !
  INTEGER ::  MAX_UNCOMPRESS_BLOCK   = 500000
  INTEGER ::  RAD_IMG_HDR_LEN        = 18
  INTEGER ::  RAD_IMG_PRD_DESC_LEN   = 102
  INTEGER ::  RAD_IMG_UNCOMPRESS_LEN = 18 + 102
  INTEGER ::  RAD_IMG_COMPRESS_POS   = 18 + 102

  !---------------------------------------------------------------------

  !INTEGER ::  MXERST ( 15 )		!/* Maximum # of strings in buffer */  

  !/* Global variables */

  CHARACTER (LEN=15) :: errmsg(133)
  !				/* Error message string array */

  INTEGER ::  nermsg = 0
  !				/* Number of error messages */

  !/*
  ! * DIRENT.H (formerly DIRLIB.H)
  ! * This file has no copyright assigned and is placed in the Public Domain.
  ! * This file is a part of the mingw-runtime package.
  ! * No warranty is given; refer to the file DISCLAIMER within the package.
  ! *
  ! */

  INTEGER(2) :: d_reclen
  INTEGER(2) :: d_namlen
  INTEGER :: DIRENT_H_
  INTEGER :: DIR
  INTEGER :: cdecl_
  INTEGER :: IR_T
  INTEGER :: ruct
  CHARACTER (LEN=200) :: d_name

  !/* wide char versions */
  TYPE :: DIRENT 
     INTEGER :: d_ino		!	/* Always zero. */
     INTEGER(2) :: d_reclen	!	/* Always zero. */
     INTEGER(2) :: d_namlen	!	/* Length of name in d_name. */
     CHARACTER (LEN=200) :: d_name  ! /* File name. */
  END TYPE DIRENT

  !/*
  ! * This is an internal data structure. Good programmers will not use it
  ! * except as an argument to one of the functions below.
  ! * dd_stat field is now int (was short in older versions).
  ! */

  TYPE :: DIR_T 
     !	/* disk transfer area for this dir */
     !    TYPE (FINDDATA_T) :: dd_dta 

     !	/* dirent struct to return from dir (NOTE: this makes this thread
     !	 * safe as long as only one thread uses a particular DIR struct at
     !	 * a time) */
     TYPE (DIRENT) :: dd_dir 

     !	/* _findnext handle */
     INTEGER :: dd_handle 

     !	/*
     !         * Status of search:
     !	 *   0 = not started yet (next entry to read is first entry)
     !	 *  -1 = off the end
     !	 *   positive = 0 based index of next entry
     !	 */
     INTEGER :: dd_stat 

     !	/* given path for dir with search pattern (struct is extended) */
     CHARACTER (LEN=1) :: dd_name 
  END TYPE DIR_T

  TYPE (DIR_T)  :: pendir
  TYPE (DIR_T)  :: const
  TYPE (DIR_T)  :: har
  TYPE (DIRENT) :: cdecl
  TYPE (DIRENT) :: eaddir
  TYPE (DIRENT) :: struct

  !/* wide char versions */
  TYPE :: WDIRENT 
     INTEGER :: d_ino		!	/* Always zero. */
     INTEGER(2) :: d_reclen	!	/* Always zero. */
     INTEGER(2) :: d_namlen	!	/* Length of name in d_name. */
  END TYPE WDIRENT

  !/*
  ! * This is an internal data structure. Good programmers will not use it
  ! * except as an argument to one of the functions below.
  ! */
  TYPE :: WDIR_T 
     !	/* disk transfer area for this dir */
     !    TYPE (WFINDDATA_T) :: dd_dta 

     !	/* dirent struct to return from dir (NOTE: this makes this thread
     !	 * safe as long as only one thread uses a particular DIR struct at
     !	 * a time) */
     TYPE (WDIRENT) :: dd_dir 
     !	/* _findnext handle */
     INTEGER :: dd_handle 
     !	/*
     !        * Status of search:
     !	 *   0 = not started yet (next entry to read is first entry)
     !	 *  -1 = off the end
     !	 *   positive = 0 based index of next entry
     !	 */
     INTEGER :: dd_stat 
     !	/* given path for dir with search pattern (struct is extended) */
     CHARACTER (LEN=1) :: dd_name 

  END TYPE WDIR_T

  TYPE (WDIR_T)  :: WDIR
  TYPE (WDIRENT) :: cdecl__
  TYPE (WDIRENT) :: wreaddir
  TYPE (WDIRENT) :: WDIR_

  TYPE :: ISTAT 
     INTEGER :: st_dev       ! statb(1)
     INTEGER :: st_ino       ! statb(2)
     INTEGER :: st_mode      ! statb(3)
     INTEGER :: st_nlink     ! statb(4)
     INTEGER :: st_uid       ! statb(5)
     INTEGER :: st_gid       ! statb(6)
     INTEGER :: st_rdev      ! statb(7)
     INTEGER :: st_size      ! statb(8)
     INTEGER :: st_blocks    ! statb(?)
     INTEGER :: st_atime     ! statb(9)
     INTEGER :: st_mtime     ! statb(10)
     INTEGER :: st_ctime     ! statb(11)
     INTEGER :: st_blksize   ! statb(12)

  END TYPE ISTAT

  !> from proto_cmd.h
  !typedef int Handle;
  TYPE :: Handle
  END TYPE Handle

  INTEGER :: FALSE=0
  INTEGER :: TRUE=1

  TYPE :: F_gpc_op					  !   /* Set operation type                */
     INTEGER :: GPC_DIFF=0			  !	/* Difference                        */
     INTEGER :: GPC_INT=1				!	/* Intersection                      */
     INTEGER :: GPC_XOR=2	      !***not used	!	/* Exclusive or                      */
     INTEGER :: GPC_UNION=3			!	/* Union                             */
  END TYPE F_gpc_op

  TYPE :: vertex_type				  !	/* Edge intersection classes         */
     INTEGER :: NUL=0						!	/* Empty non-intersection            */
     INTEGER :: EMX=1						!	/* External maximum                  */
     INTEGER :: ELI=2						!	/* External left intermediate        */
     INTEGER :: TED=3						!	/* Top edge                          */
     INTEGER :: ERI=4						!	/* External right intermediate       */
     INTEGER :: RED=5						!	/* Right edge                        */
     INTEGER :: IMM=6						!	/* Internal maximum and minimum      */
     INTEGER :: IMN=7						!	/* Internal minimum                  */
     INTEGER :: EMN=8						!	/* External minimum                  */
     INTEGER :: EMM=9						!	/* External maximum and minimum      */
     INTEGER :: LED=10						!	/* Left edge                         */
     INTEGER :: ILI=11						!	/* Internal left intermediate        */
     INTEGER :: BED=12						!	/* Bottom edge                       */
     INTEGER :: IRI=13						!	/* Internal right intermediate       */
     INTEGER :: IMX=14						!	/* Internal maximum                  */
     INTEGER :: FUL=15						!	/* Full non-intersection             */
  END TYPE vertex_type

  TYPE :: h_state							!	/* Horizontal edge states            */
     INTEGER :: NH=0						    !	/* No horizontal edge                */
     INTEGER :: BH=1						    !	/* Bottom horizontal edge            */
     INTEGER :: TH=2						    !	/* Top horizontal edge               */
  END TYPE h_state

  TYPE :: F_bundle_state						!	/* Edge bundle state                 */
     INTEGER :: UNBUNDLED=0				    !	/* Isolated edge not within a bundle */
     INTEGER :: BUNDLE_HEAD=1				!	/* Bundle head node                  */
     INTEGER :: BUNDLE_TAIL=2				!	/* Passive bundle tail node          */
  END TYPE F_bundle_state

  TYPE :: F_GPC_VERTEX !CLO,CGR LIB					!	/* Polygon vertex structure          */
     REAL(8) :: x									!	/* Vertex x component                */
     REAL(8) :: y									!	/* vertex y component                */
  END TYPE F_GPC_VERTEX

  TYPE :: F_GPC_VERTEX_LIST !CLO,CGR LIB				!	/* Vertex list structure             */
     INTEGER :: num_vertices							!	/* Number of vertices in list        */
     TYPE (F_GPC_VERTEX),ALLOCATABLE :: vertex(:)		!	/* Vertex array pointer              */
  END TYPE F_GPC_VERTEX_LIST

  TYPE :: F_GPC_POLYGON !CGR LIB						!	/* Polygon set structure             */
     INTEGER :: num_contours							!	/* Number of contours in polygon     */
     INTEGER :: hole			! added					!	/* Hole / external contour flags     */
     INTEGER,ALLOCATABLE :: holea(:)	   ! changed		!	/* Hole / external contour flags     */
     TYPE (F_GPC_VERTEX_LIST),ALLOCATABLE :: contour(:)	!	/* Contour array pointer             */
  END TYPE F_GPC_POLYGON

  !****************************************************************************

  PARAMETER (IMMEDIATE = 0)
  PARAMETER (ONESHOT = 1)
  PARAMETER (DELAYED = 2)
  PARAMETER (MODE_UNDEF = 3)

  !************************************************************************
  !* ERROR.PRM								*
  !*									*
  !* This is the error parameter file.  It assigns names to error numbers. 
  !**									*
  !************************************************************************
  !------------------------------------------------------------------------
  PARAMETER	( NEVENT =   +3 )
  !						Events not supported
  PARAMETER	( NDWTBL =   +2 )
  !						No dwell table
  PARAMETER	( NCTRLP =   +1 )
  !						Broke out of contour loop
  PARAMETER	( NORMAL =    0 )
  !						Normal
  PARAMETER	( NMBRER =   -1 )
  !						Mailbox read
  PARAMETER	( NMBWER =   -2 )
  !						Mailbox write
  PARAMETER	( NEXQUO =   -3 )
  !						Exceed quota
  PARAMETER	( NFNCCD =   -4 )
  !						Invalid function code
  PARAMETER	( NOMFIL =   -5 )
  !						No map file
  PARAMETER	( NOCORD =   -6 )
  !						Invalid coordinate system
  PARAMETER	( NOBUFF =   -7 )
  !						G buffer length exceeded
  PARAMETER	( NDVICE =   -8 )
  !						Device not set
  PARAMETER	( NDCHAR =   -9 )
  !						No device characteristics
  PARAMETER	( NIVIEW =  -10 )
  !						Invalid view region
  PARAMETER	( NOLUN  =  -11	)
  !						Invalid logical unit number
  PARAMETER	( NIMODE =  -12 )
  !						Invalid mode
  PARAMETER	( NINVAL =  -13 )
  !						Invalid input parameter
  PARAMETER	( NOGRAF =  -14 )
  !						No graph defined
  PARAMETER	( NIPROJ =  -15 )
  !						Invalid projection
  PARAMETER	( NIPBND =  -16 )
  !						Invalid projection bnds
  PARAMETER	( NIPOSN =  -17 )
  !						Invalid position in proj
  PARAMETER	( NIGDSZ =  -18 )
  !						Invalid grid size
  PARAMETER	( NOPFIL =  -19	)
  !						No plot file
  PARAMETER	( NGINIT =  -20 )
  !						GINITP not called
  PARAMETER	( NODEVC =  -21 )
  !						Invalid device selected
  PARAMETER	( NICOLR =  -22 )
  !						Invalid color name selected
  PARAMETER	( NOCOLR =  -23 )
  !						Invalid color component
  PARAMETER	( NOCTBL =  -24 )
  !						No color table
  PARAMETER	( NICNUM =  -25 )
  !						Invalid color number
  PARAMETER	( NOGFIL =  -26 )
  !						No graphics file
  PARAMETER	( NSATNV =  -27 )
  !						Sat nav not defined
  PARAMETER	( NOPNTS =  -28 )
  !						Not enough points
  PARAMETER	( NIPNTS =  -29 )
  !						Too many points
  PARAMETER	( NOMONO =  -30 )
  !						Points not monotonic
  PARAMETER	( NOIMGF =  -31 )
  !						Invalid image file
  PARAMETER	( NONAVF =  -32 )
  !						Invalid navigation file
  PARAMETER	( NOMAPP =  -33 )
  !						No plotable points
  PARAMETER	( NOBNDS =  -34 )
  !						No grid points in range
  PARAMETER	( NMAPFR =  -35 )
  !						Map file read error
  PARAMETER	( NLBLEX =  -36 )
  !						Num of labels exceeds 530
  PARAMETER	( NILOGP =  -37 )
  !						Invalid LOG axis
  PARAMETER	( NNPNTS = -38  )
  !						Too many points for fill
  PARAMETER	( NWSIZE = -39 )
  !						Device has been resized
  PARAMETER	( NOINIT = -40 )
  !						GINITP not called
  PARAMETER	( NGTPNT = -41 )
  !						Error getting cursor point
  PARAMETER	( NOMETA = -42 )
  !						Error opening metafile
  PARAMETER	( NMAXFR = -43 )
  !						Too many frames
  PARAMETER	( NMDATA = -44 )
  !						Too much data for metafile
  PARAMETER	( NWINDW = -45 )
  !						Maximum number of windows open
  PARAMETER	( NCLRAL = -46 )
  !						Color cell allocation failure
  PARAMETER	( NIWNAM = -47 )
  !						Invalid window name
  PARAMETER	( NIMGFL = -48 )
  !						Can not open image file
  PARAMETER	( NIMGTBL= -49 )
  !						Can not find image table file
  PARAMETER	( NIMGENT= -50 )
  !						Cannot find the image table entry
  PARAMETER	( NWUSED = -51 )
  !						Window is already in use
  PARAMETER	( NMEMRY = -52 )
  !						Memory allocation failure
  PARAMETER	( BADATOM= -53 )
  !						Error of interning an atom
  PARAMETER	( NSRDAT = -54 )
  !						Can not get shared data
  PARAMETER	( NGRAFCOL= -55 )
  !						Not enough graphic colors
  PARAMETER	( NIMGTYP = -56 )
  !						Unknown image type
  PARAMETER	( NLUTFL = -57 )
  !						Can not find lookup table file
  PARAMETER	( NFILENM= -58 )
  !						File name is too long
  PARAMETER	( NIMGCOL= -59 )
  !						Not enough image colors
  PARAMETER	( NIMCORD = -60 )
  !						Bad image coordinates
  PARAMETER	( NIMGFMT = -61 )
  !						Invalid image format
  PARAMETER	( ZEROCB  = -62 )
  !						Zero colors specified
  PARAMETER	( NIDSIZ  = -63 )
  !						Invalid device size
  PARAMETER	( NCBALOC = -64 )
  !						Color bank not allocated
  PARAMETER	( NEWWIN  = -65 )
  !						New window was created
  PARAMETER	( NOPSFL  = -66 )
  !						Error opening PS file
  PARAMETER	( NOCLOS  = -67 )
  !						Cannot close last window
  PARAMETER	( NOFNTFL = -68 )
  !						No font file found
  PARAMETER	( NOBITMP = -69 )
  !						No open bitmap found
  PARAMETER	( NORDOPN = -70 )
  !						Could not open raster for read
  PARAMETER	( NOWROPN = -71 )
  !						Could not open 6 bit for write
  PARAMETER	( NOPROD  = -72 )
  !						No matching product in table
  PARAMETER	( NOTBL   = -73 )
  !						Product table not found
  PARAMETER	( NOEDGE  = -74 )
  !						Missing Edge table
  PARAMETER	( NOISCHD = -75 )
  !						No matching record in ISCHED array
  PARAMETER	( BADSUB  = -76 )
  !						Bad or invalid subset requested 
  PARAMETER	( BADPXV  = -77 )
  !						Bad min/max pixel values in image table
  PARAMETER	( NOUTFL  = -78 )
  !						Cannot open UTF output file
  PARAMETER	( NAFSMX  = -79 )
  !						Maximum size for AFOS file exceeded
  PARAMETER	( NROAM   = -80 )
  !						Roam window out of pixmap bounds
  PARAMETER	( NNMAPPT = -81 )
  !						Too many points in map
  PARAMETER	( NNMAPPL = -82 )
  !						Too many polygons in map
  PARAMETER	( NDISP   = -83 )
  !						DISPLAY not set or invalid
  !------------------------------------------------------------------------
  PARAMETER	( NOPROC = -101 )
  !						Nonexistent executable
  PARAMETER	( NSYSTM = -102 )
  !						System service error
  PARAMETER	( NSTRER = -103 )
  !						String error
  PARAMETER	( NNODEV = -104 )
  !						Nonexistent device
  PARAMETER	( NDEVNA = -105 )
  !						Device not allocated
  PARAMETER	( NNOOWN = -106 )
  !						Device already allocated
  PARAMETER	( NXDBUF = -107 )
  !						D buffer length exceeded
  !------------------------------------------------------------------------
  PARAMETER	( FLENGF = -120 )
  !!						File Extension not .gif
  PARAMETER	( NOCWIN = -121 )
  !						No current X Window displayed
  !------------------------------------------------------------------------



CONTAINS
  FUNCTION ERRMISS (xxxx)
    !C**********************************************************************
    !C*                                                                    *
    !C*  Function statement for missing value test.  When using this       *
    !C*  test it is important to remember to include this file AFTER       *
    !C*  all declarations, parameter statements, and SAVE statements, but  *
    !C*  BEFORE any DATA statements (for IRIX compatibility).  It is also  *
    !C*  necessary to include the file (GEMPRM.PRM) that contains the      *
    !C*  parameter statements for the missing data values (RMISSD AND      *
    !C*  RDIFFD).                                                          *
    !C**                                                                   *
    !C* Log:                                                               *
    !C* J. Wu/GSC       08/00       documented about IRIX compatibility    * 
    !C**********************************************************************
    !C

    LOGICAL	ERRMISS
    !	LOGICAL :: ERRMISS (xxxx) = ( ABS ( xxxx - RMISSD ) .lt. RDIFFD )
    ERRMISS = ( ABS ( xxxx - RMISSD ) .lt. RDIFFD )

  END FUNCTION ERRMISS

  !***********************************************************************
  !***********************************************************************
  !***********************************************************************
  ! F90 routines by: David Frank, Alan Miller, Jean Vezina
  ! must have compatible interfaces supplied in C2F.FI

  ! -----------------------------
  FUNCTION ACCESS(filename, mode) RESULT(OK)
    ! -----------------------------
    ! Translation of C function access.
    ! mode = 0 check that file exists
    !      = 1 check if it is executable
    !      = 2 can file be written?
    !      = 4 can file be read?
    !      = 6 both of the above
    ! access returns a .FALSE. result if mode is not one of above

    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(IN)           :: mode
    LOGICAL                       :: OK

    ! Local variables
    INTEGER :: length
    CHARACTER (LEN=7) :: r, w, rw

    SELECT CASE (mode)
    CASE (0)
       INQUIRE(FILE=filename, EXIST=OK)
    CASE (1)
       length = LEN_TRIM(filename)
       IF(length < 5) THEN
          OK = .FALSE.
       ELSE
          OK = filename(length-3:length) =='.exe' .OR. &
               filename(length-3:length) =='.EXE'
       END IF
    CASE (2)
       INQUIRE(FILE=filename, WRITE=w)
       OK = (w == 'YES')
    CASE (4)
       INQUIRE(FILE=filename, READ=r)
       OK = (r == 'YES')
    CASE (6)
       INQUIRE(FILE=filename, READWRITE=rw)
       OK = (rw == 'YES')
    CASE DEFAULT
       OK = .FALSE.
    END SELECT

    RETURN
  END FUNCTION ACCESS

  !**********************************************************************
  ! ---------------------------
  FUNCTION CEIL (value) RESULT(x)
    ! ---------------------------
    IMPLICIT NONE
    REAL(4) :: value, x
    x = CEILING(value)
  END FUNCTION CEIL

  !**********************************************************************
  ! ---------------------------
  FUNCTION FEOF (unitnum) RESULT(eof)
    ! ---------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: unitnum
    LOGICAL             :: eof
    INTEGER             :: status

    INQUIRE(unitnum, IOSTAT=status)
    eof = (status < 0)
    RETURN
  END FUNCTION FEOF

  !**********************************************************************
  ! ---------------------------
  FUNCTION F_FOPEN (path, action)  RESULT (unit)
    ! ---------------------------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: path, action
    INTEGER :: unit

    CHARACTER (LEN=10) :: access, form, mode
    INTEGER :: iocheck, num
    LOGICAL :: unitopen

    INTEGER :: IFILENUM,I
    LOGICAL :: LFILENUM(100)
    COMMON/FILEINFO/IFILENUM,LFILENUM

    !set file open defaults
    access = 'SEQUENTIAL'
    form   = 'FORMATTED '
    mode   = 'READWRITE '


    IF (action == 'a'.OR.action == 'a+') THEN
       access = 'APPEND'
    ELSE IF (action == 'r'.OR.action == 'rt') THEN
       mode = 'READ'
    ELSE IF (action == 'rb') THEN
       mode = 'READ'
       form = 'BINARY'
    ELSE IF (action == 'r+'.OR.action == 'w+') THEN
       mode = 'READWRITE'
    ELSE IF (action == 'w'.OR.action == 'wt') THEN
       mode = 'WRITE'
    ELSE IF (action == 'wb') THEN
       mode = 'WRITE'
       form = 'BINARY'
    END IF

    !  DO num = 11,99                         ! find 1st avail. unit
    !     INQUIRE (UNIT=num,OPENED=unitopen)
    !     IF (.NOT.unitopen) EXIT             ! num is avail.
    !  END DO

    !
    !  TO FIND AN AVAILABLE FILE POINTER, CYCLE THROUGH
    !  LFILENUM. THE FIRST AVAILABLE (.FALSE.) WILL BE USED 
    !  & SET TO .TRUE. SO IT CAN'T BE USED BY ANY OTHER
    !  TASKS.
    !
    DO num=100,200
       IF(.NOT.LFILENUM(num)) THEN
          IFILENUM = num
          LFILENUM(num) = .TRUE.
          EXIT
       ENDIF
    ENDDO

    OPEN (UNIT=num,FILE=path,ACCESS=access,FORM=form,ACTION=mode,IOSTAT=iocheck)

    IF (iocheck == 0) THEN      ! open successful
       unit = num               ! return a positive num 11,12,13,,,,
    ELSE
       unit = 0                 ! return 0 if err
       IFILENUM = 0
       LFILENUM(num) = .FALSE.
    END IF

  END FUNCTION F_FOPEN

  !**********************************************************************
  ! -----------------------------
  SUBROUTINE FFLUSH (unitnum)
    ! -----------------------------
    !  USE DFLIB, ONLY: COMMITQQ
    USE IFCORE, ONLY: COMMITQQ
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: unitnum
    LOGICAL :: result

    result = COMMITQQ (unitnum)
  END SUBROUTINE FFLUSH

  !**********************************************************************
  ! ---------------------------
  FUNCTION FGETC (unit,ch)  RESULT(status)
    ! ---------------------------
    IMPLICIT NONE
    INTEGER   :: unit, status
    CHARACTER :: ch

    ! assumes file opened with "rb" read binary
    READ (unit,IOSTAT=status,ERR=1,END=1) ch
1   CONTINUE
    IF (status > 0) WRITE (*,91) 'read unit',unit,' status=',status
    RETURN
91  FORMAT (A,I3,A,I4)
  END FUNCTION FGETC

  !**********************************************************************
  ! -----------------------------
  FUNCTION TOUPPER (ch) RESULT(outchar)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    CHARACTER (LEN=1)             :: outchar

    ! This simple version assumes that the lower case characters occur
    ! 32 positions before upper case characters, as in the ASCII collating
    ! sequence.   For a more general solution, substitute for the '32'
    ! ICHAR('a') - ICHAR('A')

    IF ( ch >= 'a' .AND. ch <= 'z') THEN
       outchar = CHAR(ICHAR(ch) -32)
    ELSE
       outchar = ch
    END IF
  END FUNCTION TOUPPER

  !**********************************************************************
  ! -----------------------------
  FUNCTION TOLOWER (ch) RESULT(outchar)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    CHARACTER (LEN=1)             :: outchar

    ! This simple version assumes that the lower case characters occur
    ! 32 positions before upper case characters, as in the ASCII collating
    ! sequence.   For a more general solution, substitute for the '32'
    ! ICHAR('a') - ICHAR('A')

    IF ( ch >= 'A' .AND. ch <= 'Z') THEN
       outchar = CHAR(ICHAR(ch) +32)
    ELSE
       outchar = ch
    END IF
  END FUNCTION TOLOWER

  !**********************************************************************
  ! -----------------------------
  FUNCTION ISALPHA (ch)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    LOGICAL             :: isalpha

    !  Checks if a character is alphabetic

    isalpha = ch >= 'a' .and. ch <= 'z' .or.  ch >= 'A' .and. ch <= 'Z'
  END FUNCTION ISALPHA

  !**********************************************************************
  ! -----------------------------
  FUNCTION ISALNUM (ch)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    LOGICAL             :: isalnum

    !  Checks if a character is alphanumeric

    isalnum = ch >= 'a' .and. ch <= 'z' .or.  ch >= 'A' .and. ch <= 'Z' .or. &
         ch >= '0' .and. ch <= '9'
  END FUNCTION ISALNUM

  !**********************************************************************
  ! -----------------------------
  FUNCTION ISDIGIT (ch)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    LOGICAL             :: isdigit

    !  Checks if a character is a digit

    isdigit =  ch >= '0' .and. ch <= '9'
  END FUNCTION ISDIGIT

  !**********************************************************************
  ! -----------------------------
  FUNCTION ISLOWER (ch)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    LOGICAL             :: islower

    !  Checks if a character is lower case

    islower = ch >= 'a' .and. ch <= 'z'
  END FUNCTION ISLOWER

  !**********************************************************************
  ! -----------------------------
  FUNCTION ISUPPER (ch)
    ! -----------------------------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(IN) :: ch
    LOGICAL             :: isupper

    !  Checks if a character is upper case

    isupper = ch >= 'A' .and. ch <= 'Z'
  END FUNCTION ISUPPER

  !**********************************************************************
  ! -----------------------------
  ! memcpy translations for data array types
  ! -----------------------------
  SUBROUTINE COPY_I4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_I4

  !**********************************************************************
  SUBROUTINE COPY_R4(a,b,n)
    INTEGER :: n
    REAL(4) :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_R4

  !**********************************************************************
  SUBROUTINE COPY_R8(a,b,n)
    INTEGER :: n
    REAL(8) :: a(n), b(n)
    a = b
  END SUBROUTINE COPY_R8

  !**********************************************************************
  ! -----------------------------
  ! memmove translations for data array types
  ! -----------------------------
  SUBROUTINE MOVE_I4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_I4

  !**********************************************************************
  SUBROUTINE MOVE_R4(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_R4

  !**********************************************************************
  SUBROUTINE MOVE_R8(a,b,n)
    INTEGER :: n
    INTEGER :: a(n), b(n), temp(8)
    temp = b
    a = temp
  END SUBROUTINE MOVE_R8

  !**********************************************************************
  ! -----------------------------
  FUNCTION INT_CAST_TO_ARRAY_PTR(arg)
    ! -----------------------------
    INTEGER,POINTER :: INT_CAST_TO_ARRAY_PTR(:)
    INTEGER,TARGET :: arg(1)
    INT_CAST_TO_ARRAY_PTR => arg
  END FUNCTION INT_CAST_TO_ARRAY_PTR

  !**********************************************************************
  FUNCTION FLT_CAST_TO_ARRAY_PTR(arg)
    REAL(4),POINTER :: FLT_CAST_TO_ARRAY_PTR(:)
    REAL(4),TARGET :: arg(1)
    FLT_CAST_TO_ARRAY_PTR => arg
  END FUNCTION FLT_CAST_TO_ARRAY_PTR

  !**********************************************************************
  FUNCTION DBL_CAST_TO_ARRAY_PTR(arg)
    REAL(8),POINTER :: DBL_CAST_TO_ARRAY_PTR(:)
    REAL(8),TARGET :: arg(1)
    DBL_CAST_TO_ARRAY_PTR => arg
  END FUNCTION DBL_CAST_TO_ARRAY_PTR

  !**********************************************************************
  ! -----------------------------
  FUNCTION STRING_TO_PTR(string)
    ! -----------------------------
    ! Convert a constant string into a pointer to a string
    !
    CHARACTER(*),INTENT(IN) :: string
    CHARACTER,POINTER,DIMENSION(:) :: STRING_TO_PTR

    ALLOCATE(STRING_TO_PTR(LEN(string)))

    STRING_TO_PTR = TRANSFER(string,(/' '/))
  END FUNCTION STRING_TO_PTR

  !**********************************************************************
  ! -----------------------------
  FUNCTION TO_STRING(strptr)
    ! -----------------------------
    !  Convert a pointer of n characters into a string of length n
    CHARACTER,INTENT(IN) :: strptr(:)
    CHARACTER*999,PARAMETER :: filler=' '
    CHARACTER (LEN=LEN_TRIM(TRANSFER(strptr,filler(:size(strptr))))) TO_STRING
    TO_STRING = TRANSFER(strptr,TO_STRING)
  END FUNCTION TO_STRING

  !**********************************************************************
  ! -----------------------------
  !FUNCTION CV_TO_PTR(string)
  !! -----------------------------
  !! Convert a character(*) variable to a pointer to an array of n characters
  !! This function cheats a little the standard by transforming a string into
  !! an array of n characters while keeping the same starting address
  !CHARACTER(*),target::string
  !CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR
  !INTERFACE
  !   FUNCTION CV_TO_PTR2(string,len)
  !   CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR2
  !   CHARACTER(*),TARGET :: string
  !   END FUNCTION
  !END INTERFACE
  !
  !!CV_TO_PTR => CV_TO_PTR2(string,LEN(string))    !*****fix later
  !
  !END FUNCTION

  !**********************************************************************
  ! -----------------------------
  FUNCTION CV_TO_PTR2(string,ilen)
    ! -----------------------------
    !  Transform a scalar string into a 1 dimensional array of chars
    !  while keeping the same starting address
    CHARACTER,POINTER,DIMENSION(:) :: CV_TO_PTR2
    CHARACTER,TARGET :: string(ilen)
    CV_TO_PTR2 => string
  END FUNCTION CV_TO_PTR2

  !**********************************************************************
  SUBROUTINE MEMSET_s(STRING,CHR,N)  ! For character strings with char chr
    CHARACTER(*) STRING,CHR*1
    STRING = repeat(CHR,N)
  END SUBROUTINE MEMSET_s

  !**********************************************************************
  SUBROUTINE MEMSET_a(STRING,CHR,N)  ! For character arrays with char chr
    CHARACTER(1) STRING(N),CHR
    STRING = CHR
  END SUBROUTINE MEMSET_a

  !**********************************************************************
  SUBROUTINE MEMSET_si(STRING,CHR,N) ! For character strings with int chr
    CHARACTER(*) STRING
    INTEGER CHR
    STRING = repeat(CHAR(CHR),N)
  END SUBROUTINE MEMSET_si

  !**********************************************************************
  SUBROUTINE MEMSET_ai(STRING,CHR,N) ! For character arrays with int chr
    CHARACTER(1) STRING(N)
    INTEGER CHR
    STRING = CHAR(CHR)
  END SUBROUTINE MEMSET_ai

  !**********************************************************************
  FUNCTION STRCMP(s1,s2)     RESULT(output_4)
    CHARACTER (LEN=*) :: s1,s2
    INTEGER :: output_4

    IF (s1 < s2) THEN
       output_4 = -1
    ELSE IF (s1 > s2) THEN
       output_4 = +1
    ELSE
       output_4 = 0
    END IF
    RETURN
  END FUNCTION STRCMP

  !**********************************************************************
  FUNCTION STRNCMP(s1,s2,n)     RESULT(output_4)
    CHARACTER (LEN=*) :: s1,s2
    INTEGER :: n, output_4

    IF (s1(1:n) < s2(1:n)) THEN
       output_4 = -1
    ELSE IF (s1(1:n) > s2(1:n)) THEN
       output_4 = +1
    ELSE
       output_4 = 0
    END IF
    RETURN
  END FUNCTION STRNCMP

END MODULE GEMPRM
