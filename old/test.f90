program main
implicit none
real, allocatable :: delta(:)
integer :: filelength=0
integer :: i, j, io
CHARACTER(len=8) :: output

do j=1,3

write(output,"(i4)") j
output='POSCAR'//trim(adjustl(output))
write (*,*) output
enddo

endprogram
