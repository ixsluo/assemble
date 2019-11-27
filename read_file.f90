! This subroutine read the input.dat
subroutine read_input(input)
use parameters
implicit none
character(len=80)              :: line, front, back
integer                        :: io=0
type(input_type)               :: input

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
character(len=40), intent(in)            :: posfile
type(pos_type)                           :: pos
integer, intent(in)                      :: nspecies
integer                                  :: io

write (*,*) 'Reading ', posfile

open (unit=11, file=posfile, action='read', iostat=io)
  if (io/=0) then
     write (*,*) 'wrong POSCAR'
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


endsubroutine
