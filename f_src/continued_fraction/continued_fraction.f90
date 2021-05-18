subroutine i4cf_evaluate ( n, a, b, p, q )

!*****************************************************************************80
!
!! I4CF_EVALUATE evaluates a continued fraction with I4 entries.
!
!  Discussion:
!
!    For convenience, we omit the parentheses or multiline display.
!
!    CF = A(0) + B(1) / A(1) + B(2) / A(2) + ... A(N-1) + B(N) / A(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    06 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, Charles Mesztenyi,
!    John Rice, Henry Thatcher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of continued fraction
!    coefficients.
!
!    Input, integer ( kind = 4 ) A(0:N), B(0:N), the continued fraction 
!    coefficients.
!
!    Output, integer ( kind = 4 ) P(0:N), Q(0:N), the N+1 successive 
!    approximations to the value of the continued fraction.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(0:n)
  integer ( kind = 4 ) b(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:n)
  integer ( kind = 4 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1 + 0
      q(i) = a(i) * 0 + 1
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + b(i) * 1
      q(i) = a(i) * q(i-1) + b(i) * 0
    else
      p(i) = a(i) * p(i-1) + b(i) * p(i-2)
      q(i) = a(i) * q(i-1) + b(i) * q(i-2)
    end if

  end do

  return
end
subroutine i4cf_evaluate_test ( )

!*****************************************************************************80
!
!! I4CF_EVALUATE_TEST tests I4CF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    07 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 4 ) :: a(0:n) = (/ &
    3, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6 /)
  integer ( kind = 4 ) :: b(0:n) = (/&
      0, &
      1,    9,   25,   49,   81, &
    121,  169,  225,  289,  361, &
    441,  529,  625,  729,  841, &
    961, 1089, 1225, 1369 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:n)
  integer ( kind = 4 ) q(0:n)
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4CF_EVALUATE_TEST:'

  call i4cf_evaluate ( n, a, b, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  CF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine i4scf_evaluate ( n, a, p, q )

!*****************************************************************************80
!
!! I4SCF_EVALUATE evaluates a simple continued fraction with I4 entries.
!
!  Discussion:
!
!    The simple continued fraction with integer coefficients is:
!
!      SCF = A(0) + 1 / ( A(1) + 1 / ( A(2) ... + 1 / A(N) ) )
!
!    This routine returns the successive approximants P(I)/Q(I)
!    to the value of the rational number represented by the continued
!    fraction, with the value exactly equal to the final ratio P(N)/Q(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, Charles Mesztenyi,
!    John Rice, Henry Thatcher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of continued fraction
!    coefficients.
!
!    Input, integer ( kind = 4 ) A(0:N), the continued fraction coefficients.
!
!    Output, integer ( kind = 4 ) P(0:N), Q(0:N), the numerators and
!    denominators of the successive approximations.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:n)
  integer ( kind = 4 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1 + 0
      q(i) = a(i) * 0 + 1
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + 1
      q(i) = a(i) * q(i-1) + 0
    else
      p(i) = a(i) * p(i-1) + p(i-2)
      q(i) = a(i) * q(i-1) + q(i-2)
    end if

  end do

  return
end
subroutine i4scf_evaluate_test ( )

!*****************************************************************************80
!
!! I4SCF_EVALUATE_TEST tests I4SCF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 4 ) :: a(0:n) = (/ &
    3, 7, 15, 1, 292, &
    1, 1,  1, 2,   1, &
    3, 1, 14, 2,   1, &
    1, 2,  2, 2,   2 /)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:n)
  integer ( kind = 4 ) q(0:n)
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4SCF_EVALUATE_TEST:'

  call i4scf_evaluate ( n, a, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SCF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine i8cf_evaluate ( n, a, b, p, q )

!*****************************************************************************80
!
!! I8CF_EVALUATE evaluates a continued fraction with I8 entries.
!
!  Discussion:
!
!    For convenience, we omit the parentheses or multiline display.
!
!    CF = A(0) + B(1) / A(1) + B(2) / A(2) + ... A(N-1) + B(N) / A(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    05 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, Charles Mesztenyi,
!    John Rice, Henry Thatcher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of continued fraction
!    coefficients.
!
!    Input, integer ( kind = 8 ) A(0:N), B(0:N), the continued fraction 
!    coefficients.
!
!    Output, integer ( kind = 8 ) P(0:N), Q(0:N), the N successive 
!    approximations to the value of the continued fraction.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 8 ) a(0:n)
  integer ( kind = 8 ) b(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 8 ) p(0:n)
  integer ( kind = 8 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1 + 0
      q(i) = a(i) * 0 + 1
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + b(i) * 1
      q(i) = a(i) * q(i-1) + b(i) * 0
    else
      p(i) = a(i) * p(i-1) + b(i) * p(i-2)
      q(i) = a(i) * q(i-1) + b(i) * q(i-2)
    end if

  end do

  return
end
subroutine i8cf_evaluate_test ( )

!*****************************************************************************80
!
!! I8CF_EVALUATE_TEST tests I8CF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    05 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 8 ) :: a(0:n) = (/ &
    3, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, &
    6, 6, 6, 6 /)
  integer ( kind = 8 ) :: b(0:n) = (/&
      0, &
      1,    9,   25,   49,   81, &
    121,  169,  225,  289,  361, &
    441,  529,  625,  729,  841, &
    961, 1089, 1225, 1369 /)
  integer ( kind = 4 ) i
  integer ( kind = 8 ) p(0:n)
  integer ( kind = 8 ) q(0:n)
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I8CF_EVALUATE_TEST:'

  call i8cf_evaluate ( n, a, b, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  CF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine i8scf_evaluate ( n, a, p, q )

!*****************************************************************************80
!
!! I8SCF_EVALUATE evaluates a simple continued fraction with I8 entries.
!
!  Discussion:
!
!    The simple continued fraction with integer coefficients is:
!
!      SCF = A(0) + 1 / ( A(1) + 1 / ( A(2) ... + 1 / A(N) ) )
!
!    This routine returns the successive approximants P(I)/Q(I)
!    to the value of the rational number represented by the continued
!    fraction, with the value exactly equal to the final ratio P(N)/Q(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, Charles Mesztenyi,
!    John Rice, Henry Thatcher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of continued fraction
!    coefficients.
!
!    Input, integer ( kind = 8 ) A(0:N), the continued fraction coefficients.
!
!    Output, integer ( kind = 8 ) P(0:N), Q(0:N), the numerators and
!    denominators of the successive approximations.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 8 ) p(0:n)
  integer ( kind = 8 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1 + 0
      q(i) = a(i) * 0 + 1
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + 1
      q(i) = a(i) * q(i-1) + 0
    else
      p(i) = a(i) * p(i-1) + p(i-2)
      q(i) = a(i) * q(i-1) + q(i-2)
    end if

  end do

  return
end
subroutine i8scf_evaluate_test ( )

!*****************************************************************************80
!
!! I8SCF_EVALUATE_TEST tests I8SCF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 8 ) :: a(0:n) = (/ &
    3, 7, 15, 1, 292, &
    1, 1,  1, 2,   1, &
    3, 1, 14, 2,   1, &
    1, 2,  2, 2,   2 /)
  integer ( kind = 4 ) i
  integer ( kind = 8 ) p(0:n)
  integer ( kind = 8 ) q(0:n)
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I8SCF_EVALUATE_TEST:'

  call i8scf_evaluate ( n, a, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SCF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine i8vec_print ( n, a, title )

!*****************************************************************************80
!
!! I8VEC_PRINT prints an I8VEC.
!
!  Discussion:
!
!    An I8VEC is a vector of I8's.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i24)' ) i, ':', a(i)
  end do

  return
end
subroutine r8_to_i4scf ( r, n, a )

!*****************************************************************************80
!
!! R8_TO_I4SCF approximates an R8 with an I4 simple continued fraction.
!
!  Discussion:
!
!    The simple continued fraction with real coefficients is:
!
!      SCF = A(0) + 1 / ( A(1) + 1 / ( A(2) ... + 1 / A(N) ) )
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Richert,
!    Strang's Strange Figures,
!    American Mathematical Monthly,
!    Volume 99, Number 2, February 1992, pages 101-107.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the real value.
!
!    Input, integer ( kind = 4 ) N, the number of convergents to compute.
!
!    Output, integer ( kind = 4 ) A(0:N), the partial quotients.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r2

  if ( r == 0.0D+00 ) then
    a(0:n) = 0
    return
  end if

  r2 = r
  a(0) = int ( r2 )

  do i = 1, n
    r2 = 1.0D+00 / ( r2 - real ( a(i-1), kind = 8 ) )
    a(i) = int ( r2 )
  end do

  return
end
subroutine r8_to_i4scf_test ( )

!*****************************************************************************80
!
!! R8_TO_I4SCF_TEST tests R8_TO_I4SCF.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 4 ) a(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p(0:n)
  integer ( kind = 4 ) q(0:n)
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_I4SCF_TEST:'

  r = r8_pi

  call r8_to_i4scf ( r, n, a )

  call i4vec_print ( n + 1, a, '  SCF coefficients:' )

  call i4scf_evaluate ( n, a, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SCF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine r8_to_i8scf ( r, n, a )

!*****************************************************************************80
!
!! R8_TO_I8SCF approximates an R8 with an I8 simple continued fraction.
!
!  Discussion:
!
!    The simple continued fraction with real coefficients is:
!
!      SCF = A(0) + 1 / ( A(1) + 1 / ( A(2) ... + 1 / A(N) ) )
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Richert,
!    Strang's Strange Figures,
!    American Mathematical Monthly,
!    Volume 99, Number 2, February 1992, pages 101-107.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the real value.
!
!    Input, integer ( kind = 4 ) N, the number of convergents to compute.
!
!    Output, integer ( kind = 8 ) A(0:N), the partial quotients.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) r2

  if ( r == 0.0D+00 ) then
    a(0:n) = 0
    return
  end if

  r2 = r
  a(0) = int ( r2, kind = 8 )

  do i = 1, n
    r2 = 1.0D+00 / ( r2 - real ( a(i-1), kind = 8 ) )
    a(i) = int ( r2, kind = 8 )
  end do

  return
end
subroutine r8_to_i8scf_test ( )

!*****************************************************************************80
!
!! R8_TO_I8SCF_EVALUATE_TEST tests R8_TO_I8SCF.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    04 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  integer ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 8 ) p(0:n)
  integer ( kind = 8 ) q(0:n)
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8_TO_I8SCF_TEST:'

  r = r8_pi

  call r8_to_i8scf ( r, n, a )

  call i8vec_print ( n + 1, a, '  SCF coefficients:' )

  call i8scf_evaluate ( n, a, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SCF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    t = real ( p(i), kind = 8 ) / real ( q(i), kind = 8 )
    write ( *, * ) i, p(i), q(i), t
  end do

  return
end
subroutine r8cf_evaluate ( n, a, b, p, q )

!*****************************************************************************80
!
!! R8CF_EVALUATE evaluates a continued fraction with R8 entries.
!
!  Discussion:
!
!    For convenience, we omit the parentheses or multiline display.
!
!    CF = A(0) + B(1) / A(1) + B(2) / A(2) + ... A(N-1) + B(N) / A(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    06 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of terms.
!
!    Input, real ( kind = 8 ) A(0:N), B(0:N), the continued fraction
!    terms.
!
!    Output, real ( kind = 8 ) P(0:N), Q(0:N), the numerators
!    and denominators of the successive partial sums of the continued
!    fraction.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  real ( kind = 8 ) b(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1.0D+00 + 0.0D+00
      q(i) = a(i) * 0.0D+00 + 1.0D+00
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + b(i) * 1.0D+00
      q(i) = a(i) * q(i-1) + b(i) * 0.0D+00
    else
      p(i) = a(i) * p(i-1) + b(i) * p(i-2)
      q(i) = a(i) * q(i-1) + b(i) * q(i-2)
    end if

  end do

  return
end
subroutine r8cf_evaluate_test ( )

!*****************************************************************************80
!
!! R8CF_EVALUATE_TEST tests R8CF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    03 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) a(0:n)
  real ( kind = 8 ) b(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) q(0:n)
  real ( kind = 8 ) t

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8CF_EVALUATE_TEST:'

  a(0) = 3.0D+00
  a(1:n) = 6.0D+00
  b(0) = 0.0D+00
  do i = 1, n
    t = real ( 2 * i - 1, kind = 8 )
    b(i) = t ** 2
  end do

  call r8cf_evaluate ( n, a, b, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  CF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    write ( *, * ) i, p(i), q(i), p(i) / q(i)
  end do

  return
end
subroutine r8scf_evaluate ( n, a, p, q )

!*****************************************************************************80
!
!! R8SCF_EVALUATE evaluates a simple continued fraction with R8 entries.
!
!  Discussion:
!
!    The simple continued fraction with real coefficients is:
!
!      SCF = A(0) + 1 / ( A(1) + 1 / ( A(2) ... + 1 / A(N) ) )
!
!    This routine returns the N successive approximants P(I)/Q(I)
!    to the value of the rational number represented by the continued
!    fraction, with the value exactly equal to the final ratio C(N)/D(N).
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    03 August 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of continued fraction
!    coefficients.
!
!    Input, real ( kind = 8 ) A(0:N), the continued fraction coefficients.
!
!    Output, real ( kind = 8 ) P(0:N), Q(0:N), the numerators and
!    denominators of the successive approximations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(0:n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) q(0:n)

  do i = 0, n

    if ( i == 0 ) then
      p(i) = a(i) * 1.0 + 0.0D+00
      q(i) = a(i) * 0.0 + 1.0D+00
    else if ( i == 1 ) then
      p(i) = a(i) * p(i-1) + 1.0D+00
      q(i) = a(i) * q(i-1) + 0.0D+00
    else
      p(i) = a(i) * p(i-1) + p(i-2)
      q(i) = a(i) * q(i-1) + q(i-2)
    end if

  end do

  return
end
subroutine r8scf_evaluate_test ( )

!*****************************************************************************80
!
!! R8SCF_EVALUATE_TEST tests R8SCF_EVALUATE.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    03 August 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 19

  real ( kind = 8 ) :: a(0:n) = (/ &
    3.0D+00, 7.0D+00, 15.0D+00, 1.0D+00, 292.0D+00, &
    1.0D+00, 1.0D+00,  1.0D+00, 2.0D+00,   1.0D+00, &
    3.0D+00, 1.0D+00, 14.0D+00, 2.0D+00,   1.0D+00, &
    1.0D+00, 2.0D+00,  2.0D+00, 2.0D+00,   2.0D+00 /)
  integer ( kind = 4 ) i
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) q(0:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8SCF_EVALUATE_TEST:'

  call r8scf_evaluate ( n, a, p, q )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  SCF numerators, denominators, ratios:'
  write ( *, '(a)' ) ''

  do i = 0, n
    write ( *, * ) i, p(i), q(i), p(i) / q(i)
  end do

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    I don't care what you do with this code.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
