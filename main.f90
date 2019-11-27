program double
! Read from input.dat and POSCAR_1 and POSCAR_2.
! Choose which axis by 1 for x, 2 for y, 3 for z.
! In pos_save, only lat[3:3], noa[:], pos_f[:,3] need to be changed.
use parameters
use math
implicit none
character(len=40)                       :: filename
type(pos_type)                          :: pos_1, pos_2         ! read in
type(pos_type)                          :: pos_save, pos_o      ! internal and write out
integer                                 :: nspecies
type(input_type)                        :: input

! read input.dat
call read_input(input)
write (*,*) 'nspecies: ', input%nspecies
write (*,*) 'ndelta: ', input%ndelta

! read POSCAR_1
filename='POSCAR_1'
call read_pos(filename, pos_1, input%nspecies) 
! read POSCAR_2
filename='POSCAR_2'
call read_pos(filename, pos_2, input%nspecies) 

! init pos_save
pos_save%sysname                    =   pos_1%sysname
pos_save%ratio                      =   pos_1%ratio
pos_save%lat                        =   pos_1%lat
pos_save%species                    =   pos_1%species
allocate ( pos_save%noa(2*input%nspecies) )
pos_save%noa(:input%nspecies)       =   pos_1%noa(:)
pos_save%noa(input%nspecies+1:)     =   pos_2%noa(:)
pos_save%symbol                     =   pos_1%symbol
allocate ( pos_save%pos_f( sum(pos_save%noa),3 ) )
! fractional to cartesian, save in pos_save%pos_f
  call coor_trans(pos_save%pos_f(:sum(pos_1%noa),1:3), pos_1%lat, pos_1%pos_f)
  call coor_trans(pos_save%pos_f(sum(pos_1%noa)+1:,1:3), pos_1%lat, pos_1%pos_f)

delta : do i=1,input%ndelta
! calculate pos_o%pos_f in cartesian and pos_o%lat


enddo delta
endprogram
