program double
! Read from input.dat and POSCAR_1 and POSCAR_2.
! Choose which axis by 1 for x, 2 for y, 3 for z.
! ----------     -----------
! |POSCAR_1|     |         |
! |vacuum  |  => |SPOSCAR_*|
! |POSCAR_2|     |         |
! ----------     -----------
!
! In pos_save, only lat[3:3], noa[:], pos_f[:,3] need to be changed.
!
! 11/27/2019   Xiaoshan Luo
use parameters
use math
implicit none
character(len=40)                       :: filename
type(pos_type)                          :: pos_1, pos_2         ! read in
type(pos_type)                          :: pos_save             ! internal and write out
type(input_type)                        :: input
real(dbl), dimension(3,3)               :: tal=0.0              ! invert lat
integer                                 :: ii, jj, kk

! read input.dat
write (*,*) 'Reading input.dat'
call read_input(input)
write (*,*) 'axis: ', input%axis
write (*,*) 'nspecies: ', input%nspecies
write (*,*) 'ndelta: ', input%ndelta
write (*,*) 'delta: ', input%delta(:)

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
pos_save%species                    =   trim(adjustl(pos_1%species))//'  '//&
                                       &trim(adjustl(pos_2%species))
allocate ( pos_save%noa(2*input%nspecies) )
pos_save%noa(:input%nspecies)       =   pos_1%noa(:)
pos_save%noa(input%nspecies+1:)     =   pos_2%noa(:)
pos_save%symbol                     =   pos_1%symbol
allocate ( pos_save%pos_f( sum(pos_save%noa),3 ) )

write (*,*) pos_save%species
write (*,*) pos_save%noa

! loop on delta
delta : do kk=1,input%ndelta
  write (*,*) 'delta: ', input%delta(kk)
  
  ! (1) fractional to cartesian, save in pos_save%pos_f
  call coor_trans(pos_save%pos_f(:sum(pos_1%noa),1:3), pos_1%lat, pos_1%pos_f)
  call coor_trans(pos_save%pos_f(sum(pos_1%noa)+1:,1:3), pos_1%lat, pos_1%pos_f)
  
  ! (2) calculate pos_o%pos_f in cartesian and pos_o%lat
  !     only cell 2 along axis input%axis should be changed
  do ii=1,sum(pos_2%noa)
    pos_save%pos_f(ii+sum(pos_1%noa),input%axis) = pos_save%pos_f(ii+sum(pos_1%noa),input%axis) &
                                                & + pos_1%lat(input%axis,input%axis) &
                                                & + input%delta(kk)
  enddo
  pos_save%lat(input%axis,input%axis) = 2*pos_save%lat(input%axis,input%axis) + input%delta(kk)

  ! (3) cartesian to fraction
  !     invert lat
  do ii=1,3
    tal(ii,ii) = 1/pos_save%lat(ii,ii)
  enddo
  call coor_trans(pos_save%pos_f(:,:), tal, pos_save%pos_f(:,:))


  ! write output
  write (*,*) 'Writing output...'
  call writeout(pos_save, input%delta(kk))
  
!!! DEBUG !!!
! ! {{{ 
!  if (kk==1) then
!    do ii=1,sum(pos_save%noa)
!      write (*,*) pos_save%pos_f(ii,:)
!    enddo
!    do ii=1,3
!      write (*,*) pos_save%lat(ii,:)
!    enddo
!  endif
!! }}}

enddo delta

write (*,*) 'Finished.'

endprogram
