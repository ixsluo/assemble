module math
contains

subroutine coor_trans(c, a, d)
! c=Ad, where A is a 3x3 diagonal matrix
use parameters
implicit none
real(dbl), dimension(:,:), intent(out)     :: c
real(dbl), dimension(:,:), intent(in)      :: d
real(dbl), dimension(:,:), intent(in)      :: a

do i=1,size(d,1)
  do j=1,3
    c(i,j) = a(j,j) * d(i,j)
  enddo
enddo

endsubroutine

endmodule
