module parameters
implicit none
integer, parameter           :: dbl = selected_real_kind(p=13)
integer                      :: i, j, k

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
  integer                                         :: nspecies
  integer                                         :: ndelta
  real(dbl), dimension(:), allocatable            :: delta
endtype input_type


endmodule
