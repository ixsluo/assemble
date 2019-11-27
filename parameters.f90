module parameters
! Parameters module
! 11/27/2019  Xiaoshan Luo
implicit none
integer, parameter       :: dbl = selected_real_kind(p=13)
real(dbl)                :: minl=0.00000001    ! a small amount
integer                  :: i, j, k

type pos_type
  character(len=40)                               :: sysname
  real(dbl)                                       :: ratio
  real(dbl), dimension(:,:), allocatable          :: lat
  character(len=80)                               :: species
  integer, dimension(:), allocatable              :: noa
  character(len=80)                               :: symbol
  real(dbl), dimension(:,:), allocatable          :: pos_f
endtype pos_type

type input_type
  integer                                         :: axis          ! along x or y or z
  integer                                         :: nspecies
  integer                                         :: ndelta
  real(dbl), dimension(:), allocatable            :: delta
endtype input_type


endmodule
