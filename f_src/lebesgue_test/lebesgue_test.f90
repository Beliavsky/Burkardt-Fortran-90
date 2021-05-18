program main

!*****************************************************************************80
!
!! MAIN is the main program for LEBESGUE_TEST.
!
!  Discussion:
!
!    LEBESGUE_TEST tests the LEBESGUE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    John Burkardt
!
  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LEBESGUE library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST01 looks at Chebyshev1 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST01:'
  write ( *, '(a)' ) '  Analyze Chebyshev1 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call chebyshev1 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Chebyshev1 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call chebyshev1 ( n, x )
  call r8vec_print ( n, x, '  Chebyshev1 points for N = 11' )

  label = 'Chebyshev1 points for N = 11'
  filename = 'chebyshev1'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST02 looks at Chebyshev2 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST02:'
  write ( *, '(a)' ) '  Analyze Chebyshev2 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call chebyshev2 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Chebyshev2 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call chebyshev2 ( n, x )
  call r8vec_print ( n, x, '  Chebyshev2 points for N = 11' )

  label = 'Chebyshev2 points for N = 11'
  filename = 'chebyshev2'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST03 looks at Chebyshev3 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST03:'
  write ( *, '(a)' ) '  Analyze Chebyshev3 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call chebyshev3 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Chebyshev3 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call chebyshev3 ( n, x )
  call r8vec_print ( n, x, '  Chebyshev3 points for N = 11' )

  label = 'Chebyshev3 points for N = 11'
  filename = 'chebyshev3'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST04 looks at Chebyshev4 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST04:'
  write ( *, '(a)' ) '  Analyze Chebyshev4 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call chebyshev4 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Chebyshev4 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call chebyshev4 ( n, x )
  call r8vec_print ( n, x, '  Chebyshev4 points for N = 11' )

  label = 'Chebyshev4 points for N = 11'
  filename = 'chebyshev4'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST05 looks at Equidistant1 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST05:'
  write ( *, '(a)' ) '  Analyze Equidistant1 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call equidistant1 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Equidistant1 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call equidistant1 ( n, x )
  call r8vec_print ( n, x, '  Equidistant1 points for N = 11' )

  label = 'Equidistant1 points for N = 11'
  filename = 'equidistant1'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST06 looks at Equidistant2 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST06:'
  write ( *, '(a)' ) '  Analyze Equidistant2 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call equidistant2 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Equidistant2 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call equidistant2 ( n, x )
  call r8vec_print ( n, x, '  Equidistant2 points for N = 11' )

  label = 'Equidistant2 points for N = 11'
  filename = 'equidistant2'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST07 looks at Equidistant3 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST07:'
  write ( *, '(a)' ) '  Analyze Equidistant3 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call equidistant3 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Equidistant3 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call equidistant3 ( n, x )
  call r8vec_print ( n, x, '  Equidistant3 points for N = 11' )

  label = 'Equidistant3 points for N = 11'
  filename = 'equidistant3'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST08 looks at Fejer 1 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST08:'
  write ( *, '(a)' ) '  Analyze Fejer1 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call fejer1 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Fejer1 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call fejer1 ( n, x )
  call r8vec_print ( n, x, '  Fejer1 points for N = 11' )

  label = 'Fejer1 points for N = 11'
  filename = 'fejer1'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! LEBESGUE_TEST09 looks at Fejer2 points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 11
  integer ( kind = 4 ), parameter :: nfun = 501

  character ( len = 255 ) filename
  real ( kind = 8 ) l(n_max)
  character ( len = 255 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) xfun(nfun)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LEBESGUE_TEST09:'
  write ( *, '(a)' ) '  Analyze Fejer2 points.'

  call r8vec_linspace ( nfun, -1.0D+00, +1.0D+00, xfun )

  do n = 1, n_max
    allocate ( x(1:n) )
    call fejer2 ( n, x )
    call lebesgue_constant ( n, x, nfun, xfun, l(n) )
    deallocate ( x )
  end do

  call r8vec_print ( n_max, l, &
    '  Fejer2 Lebesgue constants for N = 1 to 11:' )
!
!  Examine one case more closely.
!
  n = 11
  allocate ( x(1:n) )
  call fejer2 ( n, x )
  call r8vec_print ( n, x, '  Fejer2 points for N = 11' )

  label = 'Fejer2 points for N = 11'
  filename = 'fejer2'
  call lebesgue_plot ( n, x, nfun, xfun, label, filename )

  deallocate ( x )

  return
end

