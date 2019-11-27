program double
use parameters
use math
implicit none
character(len=40)                       :: filename
type(pos_type)                          :: pos_1, pos_2
type(pos_type)                          :: pos_o
integer                                 :: nspecies
type(input_type)                        :: input

! read input.dat
call read_input(input)
write (*,*) input%nspecies

! read POSCAR_1
filename='POSCAR_1'
call read_pos(filename, pos_1, input%nspecies) 
! read POSCAR_2
filename='POSCAR_2'
call read_pos(filename, pos_2, input%nspecies) 

pos_o%sysname                    =   pos_1%sysname
pos_o%ratio                      =   pos_1%ratio
pos_o%lat                        =   pos_1%lat
pos_o%species                    =   pos_1%species
allocate ( pos_o%noa(2*input%nspecies) )
pos_o%noa(:input%nspecies)       =   pos_1%noa(:)
pos_o%noa(input%nspecies+1:)     =   pos_2%noa(:)
pos_o%symbol                     =   pos_1%symbol
allocate ( pos_o%pos_f( sum(pos_o%noa),3 ) )

! fractional to cartesian, save in pos_o%pos_f
  call coor_trans(pos_o%pos_f(:sum(pos_1%noa),1:3), pos_1%lat, pos_1%pos_f)
  call coor_trans(pos_o%pos_f(sum(pos_1%noa)+1:,1:3), pos_1%lat, pos_1%pos_f)

! calculate pos_o%pos_f in cartesian and pos_o%lat

endprogram
