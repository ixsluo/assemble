! File reading and writing subroutines
! 11/27/2019   Xiaoshan Luo
subroutine read_input(input)
use parameters
implicit none
character(len=80)              :: line, front, back
integer                        :: io=0
type(input_type), intent(out)  :: input

open (unit=21, file='input.dat', action='read')
  do
    read (21,'(a80)', iostat=io) line
    if (io/=0) exit
    line = adjustl(line)

    ! filtrate null
    if ( len(line) == 0 ) cycle
    ! filtrate comments
    if ( (index(line,'#') == 1) .or. (index(line,'!') == 1) ) cycle
    ! cut by '='
    front = line(:index(line,'=')-1)
    back  = line(index(line,'=')+1:)
    front = trim(adjustl(front))
    back  = trim(adjustl(back))
    if ( len(back) == 0 ) cycle

    select case(front)
    case('axis')
      read (back,*) input%axis
    case('nspecies')
      read (back,*) input%nspecies
    case('ndelta')
      read (back,*) input%ndelta
        if ( allocated(input%delta) ) deallocate(input%delta)
        allocate ( input%delta(input%ndelta) )
    case('delta')
      read (back,*) input%delta(:)
    case default
      write (*,*) 'Illigal parameter, ignore.'
    endselect

  enddo
close(21)

endsubroutine


! This subroutine read the poscar file
subroutine read_pos(posfile, pos, nspecies)
use parameters
implicit none
character(len=40), intent(in)        :: posfile
type(pos_type),    intent(out)       :: pos
integer,           intent(in)        :: nspecies
integer                              :: io

write (*,*) 'Reading ', posfile

open (unit=11, file=posfile, action='read', iostat=io)
  if (io>0) then
    write (*,*) 'Wrong POSCAR'
  elseif (io<0) then
    write (*,*) 'End of file'
  else
    read (11,*) pos%sysname
    read (11,*) pos%ratio
    allocate ( pos%lat(3,3) )
    do i = 1, 3
      read (11,*) pos%lat(i,:)
    enddo

    read (11,*) pos%species
    
    if ( allocated(pos%noa) ) deallocate(pos%noa)
    allocate( pos%noa(nspecies) )
    read (11,*) pos%noa(:)

    read (11,*) pos%symbol
    if ( allocated(pos%pos_f) ) deallocate(pos%pos_f)
    allocate ( pos%pos_f(sum(pos%noa),3) )
    do i = 1, sum(pos%noa)
      read (11,*) pos%pos_f(i,:)
    enddo
  endif
close (11)

! orthogonal
if ( &
&  ( abs(pos%lat(1,2)) > minl ) .or. &
&  ( abs(pos%lat(1,3)) > minl ) .or. &
&  ( abs(pos%lat(2,1)) > minl ) .or. &
&  ( abs(pos%lat(2,3)) > minl ) .or. &
&  ( abs(pos%lat(3,1)) > minl ) .or. &
&  ( abs(pos%lat(3,2)) > minl ) ) then
  write (*,*) 'WORNING!!! NOT an orthognal cell, pleace change the POSCAR.'
  write (*,*) 'Stop calculating'
  stop
endif

endsubroutine


subroutine writeout(pos, delta)
use parameters
implicit none
type(pos_type), intent(in)     :: pos
real(dbl),      intent(in)     :: delta
character(len=40)              :: posfile=''
integer                        :: ii

write (posfile,*) delta
posfile = trim(adjustl(posfile))
posfile = 'SPOSCAR_'//posfile(:5)

open (unit=20, file=posfile, action='write')
  write (20,*) pos%sysname
  write (20,*) pos%ratio
  do ii=1,3
    write (20,100) pos%lat(ii,:)
  enddo
  write (20,*) pos%species
  write (20,*) pos%noa(:)
  write (20,*) pos%symbol
  do ii=1,sum(pos%noa)
    write (20,100) pos%pos_f(ii,:)
  enddo
close(20)
100 format ( 1x, 3f17.9)

endsubroutine
