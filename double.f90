PROGRAM main
! This porgram joint two orthogonal cell together
! along z axis, with a vaccum.
! Two cells have the same lattice parameters.
! --------
! POSCAR_2
! vaccum
! POACAR_1
! --------
!!! lattice should be a diagonal matrix
! INPUT  : 'POSCAR_1', 'POSCAR_2', 'input.dat'
! OUTPUT : 'SPOSCAR'
! File "input.dat" contains the values of vaccum.
!
! 11/25/2019    Xiaoshan Luo
IMPLICIT NONE
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
CHARACTER :: sysname
REAL(kind=DBL) :: ratio
REAL(kind=DBL), DIMENSION(3,3) :: lat
REAL(kind=DBL), DIMENSION(3,3) :: lat_new
REAL(kind=DBL), DIMENSION(3,3) :: tal                     ! invert lat
CHARACTER :: species
INTEGER :: noa_1
INTEGER :: noa_2
CHARACTER :: sym
! for POSCAR_1
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_f_1    ! fractional
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_c_1    ! cartesian
! for POSCAR_2
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_f_2    ! fractional
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_c_2    ! cartesian
! for SPOSCAR
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_fo     ! output frac
REAL(kind=DBL), ALLOCATABLE, DIMENSION(:,:) :: pos_co     ! output cart
INTEGER :: i, j, k
! vaccum
REAL(kind=DBL), allocatable :: delta(:)
INTEGER :: filelength=0 , io
CHARACTER(LEN=10) :: output                               ! output file name


! Read input
! {{{
! Read POSCAR_1
OPEN (UNIT=11, FILE='POSCAR_1', ACTION='READ')
  READ (11,*) sysname
  READ (11,*) ratio
  DO i=1,3
    READ (11,*) lat(i,:)
  ENDDO
  READ (11,*) species
  READ (11,*) noa_1
  READ (11,*) sym
  ALLOCATE (pos_f_1(noa_1,3))
  DO i=1,noa_1
    READ (11,*) pos_f_1(i,:)
  ENDDO
CLOSE (11)
! Read POSCAR_2
OPEN (UNIT=11, FILE='POSCAR_2', ACTION='READ')
  READ (11,*)
  READ (11,*)
  DO i=1,3
    READ (11,*) 
  ENDDO
  READ (11,*) 
  READ (11,*) noa_2
  READ (11,*) 
  ALLOCATE (pos_f_2(noa_2,3))
  DO i=1,noa_2
    READ (11,*) pos_f_2(i,:)
  ENDDO
CLOSE (11)
! Read input.dat
OPEN (UNIT=22, FILE='input.dat', ACTION='READ')
DO
  READ (22,*, IOSTAT=io)
  IF (io/=0) EXIT
  filelength = filelength + 1
ENDDO
REWIND(22)
ALLOCATE(delta(filelength))
DO i=1,filelength
  READ (22,*) delta(i)
ENDDO
CLOSE(22)
! }}}


! fractional to cartesian  
! notice that lat is a diagonal matrix
! {{{
! c_j = sum LAT_jk * d_k
! pos_f_1 -> pos_c_1
ALLOCATE (pos_c_1(noa_1,3))
DO i=1,noa_1
  DO j=1,3
    pos_c_1(i,j) =  lat(j,j)*pos_f_1(i,j)
  ENDDO
ENDDO
! pos_f_2 -> pos_c_2
ALLOCATE (pos_c_2(noa_2,3))
DO i=1,noa_2
  DO j=1,3
    pos_c_2(i,j) =  lat(j,j)*pos_f_2(i,j)
  ENDDO
ENDDO
! }}}


! Loop for delta
vaccum: DO k=1,filelength


! in cartesian
! calc pos_co and new lat
! {{{
ALLOCATE (pos_co(noa_1+noa_2,3))
DO i=1,noa_1
  pos_co(i,:) = pos_c_1(i,:)                             ! cell 1 unchang
ENDDO
DO i=1,noa_2
  pos_co(i+noa_1,1:2) = pos_c_2(i,1:2)                   ! cell 2 x and y unchange
  pos_co(i+noa_1,3) = pos_c_2(i,3) + lat(3,3) + delta(k) ! cell 2 z plus c and delta
ENDDO
lat_new(3,3) = 2*lat(3,3) + delta(k)
!}}}


! cartesian to fractional
! invert lat, notice that it is a diagonal matrix
! {{{
ALLOCATE (pos_fo(noa_1+noa_2,3))
DO i=1,3
  tal(i,i) = 1/lat(i,i)
ENDDO
DO i=1,noa_1+noa_2
  DO j=1,3
    pos_fo(i,j) = tal(j,j)*pos_co(i,j)
  ENDDO
ENDDO
!}}}

! write output
write (output,"(I4)") k
output = 'SPOSCAR_'//trim(adjustl(output))
OPEN (UNIT=12, FILE=output, STATUS='REPLACE', ACTION='WRITE')
  WRITE(12,*) sysname
  WRITE(12,*) ratio
  DO i=1,3
    WRITE(12,100) lat(i,:)
  ENDDO
  WRITE(12,*) species
  WRITE(12,*) noa_1+noa_2
  WRITE(12,*) sym
  DO i=1,noa_1+noa_2
    WRITE(12,100) pos_fo(i,:)
  ENDDO
CLOSE(12)


DEALLOCATE(pos_fo, pos_co)

ENDDO vaccum

DEALLOCATE(pos_f_1, pos_f_2, pos_c_1, pos_c_2)

100 FORMAT ( 1X, 3F17.9 )

ENDPROGRAM
