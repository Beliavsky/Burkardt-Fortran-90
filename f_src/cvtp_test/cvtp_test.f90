program main

!*****************************************************************************80
!
!! MAIN is the main program for CVTP_TEST.
!
!  Discussion:
!
!    CVTP_TEST tests the CVTP library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CVTP_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test CVTP.'

  call cvtp_modular_test ( )
  call cvtp_nonmodular_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CVTP_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine cvtp_modular_test ( )

!*****************************************************************************80
!
!! CVTP_MODULAR_TEST tests CVTP with MODULAR TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 400

  real ( kind = 8 ) change_l2
  integer ( kind = 4 ), parameter :: cvt_steps = 50
  character ( len = 80 ) :: file_out_name = 'cvtp_1x1.txt'
  real ( kind = 8 ) generator(m,n)
  integer ( kind = 4 ) i
  logical modular
  logical reset
  integer ( kind = 4 ) sample_num_cvt
  integer ( kind = 4 ), parameter :: sample_num_steps = 50
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), parameter :: seed_init = 123456789
  real ( kind = 8 ) width(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CVTP_MODULAR_TEST'
  write ( *, '(a)' ) '  CVTP can compute a periodic Centroidal Voronoi Tessellation'
  write ( *, '(a)' ) '  We set MODULAR to TRUE to do this.'

  generator(1:m,1:n) = 0.0D+00
  modular = .true.
  reset = .true.
  sample_num_cvt = 100000
  seed = 123456789
  width = (/ 1.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Spatial dimension M =        ', m
  write ( *, '(a,i12)' ) '  Number of generators =       ', n
  write ( *, '(a,l)'   ) '  MODULAR arithmetic option =  ', modular
  write ( *, '(a,i12)' ) '  Initial random number seed = ', seed
  write ( *, '(a,i6)'  ) '  Number of sample points =    ', sample_num_cvt
  write ( *, '(a,i6)'  ) '  Number of sample steps =     ', sample_num_steps
!
!  Initialize the generators.
!
  call cvtp_region_sampler ( m, n, generator, width )

  do i = 1, cvt_steps

    call cvtp_iteration ( m, n, generator, width, modular, sample_num_cvt, &
      change_l2 )

  end do

  call r8mat_write ( file_out_name, m, n, generator )

  return
end
subroutine cvtp_nonmodular_test ( )

!*****************************************************************************80
!
!! CVTP_NONMODULAR_TEST tests CVTP with MODULAR false.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 July 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2
  integer ( kind = 4 ), parameter :: n = 400

  real ( kind = 8 ) change_l2
  integer ( kind = 4 ), parameter :: cvt_steps = 50
  character ( len = 80 ) :: file_out_name = 'cvt_1x1.txt'
  real ( kind = 8 ) generator(m,n)
  integer ( kind = 4 ) i
  logical modular
  logical reset
  integer ( kind = 4 ) sample_num_cvt
  integer ( kind = 4 ), parameter :: sample_num_steps = 50
  integer ( kind = 4 ) seed
  real ( kind = 8 ) width(m)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CVTP_NONMODULAR_TEST'
  write ( *, '(a)' ) '  CVTP can compute a periodic Centroidal Voronoi Tessellation.'
  write ( *, '(a)' ) '  But here, we turn modularity OFF.'

  generator(1:m,1:n) = 0.0D+00
  modular = .false.
  reset = .true.
  sample_num_cvt = 100000
  seed = 123456789
  width = (/ 1.0D+00, 1.0D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Spatial dimension M =        ', m
  write ( *, '(a,i12)' ) '  Number of generators =       ', n
  write ( *, '(a,l)'   ) '  MODULAR arithmetic option =  ', modular
  write ( *, '(a,i12)' ) '  Initial random number seed = ', seed
  write ( *, '(a,i6)'  ) '  Number of sample points =    ', sample_num_cvt
  write ( *, '(a,i6)'  ) '  Number of sample steps =     ', sample_num_steps
!
!  Initialize the generators.
!
  call cvtp_region_sampler ( m, n, generator, width )

  do i = 1, cvt_steps
    call cvtp_iteration ( m, n, generator, width, modular, sample_num_cvt, &
      change_l2 )

  end do

  call r8mat_write ( file_out_name, m, n, generator )

  return
end
