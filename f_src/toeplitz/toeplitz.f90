function c4_abs1 ( x )

!*****************************************************************************80
!
!! C4_ABS1 computes the L1 absolute value of a C4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, complex ( kind = 4 ) X, the number whose L1 absolute value
!    is desired.
!
!    Output, real ( kind = 4 ) C4_ABS1, the L1 absolute value of X.
!
  implicit none

  real ( kind = 4 ) c4_abs1
  complex ( kind = 4 ) x

  c4_abs1 = abs ( real ( x ) ) + abs ( imag ( x ) )

  return
end
subroutine c4bto_sl ( m, l, a1, a2, b, x )

!*****************************************************************************80
!
!! C4BTO_SL solves the C4 block Toeplitz linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, complex ( kind = 4 ) A1(M*M,L), the first row of blocks of the
!    matrix.  Each block is represented by columns.
!
!    Input, complex ( kind = 4 ) A2(M*M,L-1), the first column of blocks of
!    the matrix, beginning with the second block.  Each block is represented
!    by columns.
!
!    Input, complex ( kind = 4 ) B(M*L), the right hand side vector.
!
!    Output, complex ( kind = 4 ) X(M*L), the solution vector.  X may coincide
!    with B.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a1(m*m,l)
  complex ( kind = 4 ) a2(m*m,1)
  complex ( kind = 4 ) b(m,l)
  complex ( kind = 4 ) c1(m,m,l-1)
  complex ( kind = 4 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) pivot(m)
  complex ( kind = 4 ) r1(m,m)
  complex ( kind = 4 ) r2(m,m)
  complex ( kind = 4 ) r3(m,m)
  complex ( kind = 4 ) r5(m,m)
  complex ( kind = 4 ) r6(m,m)
  complex ( kind = 4 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1
  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      r3(i,j) = r1(i,j)
      i3 = i3 + 1
    end do
    x(j,1) = b(j,1)
  end do

  call c4gefa ( r3, m, m, pivot, ii )

  call c4gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the block Toeplitz matrix
!  for N = 2, L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    i3 = 1
    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n-1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      c1(1:m,1:m,n-1) = r2(1:m,1:m)

      do i1 = 1, n - 2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            call c4vec_axpy ( m, c1(i,j,i2), a2(i3,i1), 1, r5(1,j), 1 )
            call c4vec_axpy ( m, c2(i,j,i1), a1(i3,i1+1), 1, r6(1,j), 1 )
            i3 = i3 + m
          end do
        end do
      end do

    end if

    do j = 1, m
      r2(1:m,j) = -r5(1:m,j)
      call c4gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    r3(1:m,1:m) = r6(1:m,1:m)
    r6(1:m,1:m) = -c1(1:m,1:m,1)

    do j = 1, m
      do i = 1, m
        call c4vec_axpy ( m, r2(i,j), r3(1,i), 1, c1(1,j,1), 1 )
      end do
    end do

    call c4gefa ( r6, m, m, pivot, ii )

    do j = 1, m
      call c4gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        call c4vec_axpy ( m, r3(i,j), r5(1,i), 1, r1(1,j), 1 )
      end do
    end do

    if ( 2 < n ) then

      r6(1:m,1:m) = c2(1:m,1:m,1)

      do i1 = 2, n - 1

        if ( i1 /= n - 1 ) then
          r5(1:m,1:m) = c2(1:m,1:m,i1)
        end if

        do j = 1, m
          c2(1:m,j,i1) = r6(1:m,j)
          do i = 1, m
            call c4vec_axpy ( m, r3(i,j), c1(1,i,i1), 1, c2(1,j,i1), 1 )
          end do
        end do

        do j = 1, m
          do i = 1, m
            call c4vec_axpy ( m, r2(i,j), r6(1,i), 1, c1(1,j,i1), 1 )
          end do
        end do

        r6(1:m,1:m) = r5(1:m,1:m)

      end do

    end if

    c2(1:m,1:m,1) = r3(1:m,1:m)
!
!  Compute the solution of the system with the principal minor of order M*N.
!
    r3(1:m,1:m) = r1(1:m,1:m)
    x(1:m,n) = b(1:m,n)

    do i1 = 1, n - 1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        call c4vec_axpy ( m, -x(i,i2), a2(i3,i1), 1, x(1,n), 1 )
        i3 = i3 + m
      end do
    end do

    call c4gefa ( r3, m, m, pivot, ii )

    call c4gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n - 1
      do i = 1, m
        call c4vec_axpy ( m, x(i,n), c2(1,i,i1), 1, x(1,i1), 1 )
      end do
    end do

  end do

  return
end
subroutine c4ccc_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C4CCC_SL solves the C4 double column circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M*L,K), the first row of outer blocks
!    of the CCC matrix.  Each outer block is represented by its first row
!    of inner blocks.  Each inner block is represented by its first row.
!    On return, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L*K)
!    On input, the right hand side vector.
!    On output, the solution.
!
!    Workspace, complex ( kind = 4 ) R(max(M,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  complex ( kind = 4 ) r(1)
  real ( kind = 4 ) rk
  complex ( kind = 4 ) x(m,l,k)

  rk = real ( k, kind = 4 )
  ml = m * l
!
!  Reduce the CCC matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, ml, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of the right hand side vector.
!
  call c4salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CC matrices.
!
  do i3 = 1, k
    call c4cc_sl ( a(1,i3), x(1,1,i3), r, m, l, m )
  end do
!
!  Solve the system by the inverse discrete Fourier transformation.
!
  call c4salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c4ccg_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C4CCG_SL solves the C4 CCG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M*M*L,K).
!    On input, the first row of outer blocks of the CCG matrix.
!    Each outer block is represented by its first row
!    of inner blocks.  Each inner block is represented by columns.
!    On return, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(M,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mm
  complex ( kind = 4 ) r(1)
  real ( kind = 4 ) rk
  complex ( kind = 4 ) x(m,l,k)

  rk = real ( k, kind = 4 )
  mm = m * m
  ml = m * l
!
!  Reduce the CCG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, mm * l, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c4salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CG matrices.
!
  do i3 = 1, k
    call c4cg_sl ( a(1,i3), x(1,1,i3), r, m, l, mm )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c4salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c4cc_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C4CC_SL solves the C4 column circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M,L)
!    On input, the first row of blocks of the CC matrix.
!    Each block is represented by its first row.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L)
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(M,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,l)
  integer ( kind = 4 ) i2
  complex ( kind = 4 ) r(*)
  complex ( kind = 4 ) x(m,l)
!
!  Reduce the CC matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, m, l, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c4salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are circulant matrices.
!
  do i2 = 1, l
    call c4ci_sl ( m, a(1,i2), x(1,i2) )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c4salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 4 )

  return
end
subroutine c4cct_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C4CCT_SL solves the C4 CCT linear system A * X = B..
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A((2*M-1)*L,K).
!    On input, the first row of outer blocks of the CCT matrix.
!    Each outer block is represented by its first row of inner blocks.
!    Each inner block is represented by its first row followed by its
!    first column beginning with the second element.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(2*M - 2,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) ml
  complex ( kind = 4 ) r(*)
  real ( kind = 4 ) rk
  complex ( kind = 4 ) x(m,l,k)

  rk = real ( k, kind = 4 )
  m2 = 2 * m - 1
  ml = m * l
!
!  Reduce the CCT matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, m2 * l, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c4salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CT matrices.
!
  do i3 = 1, k
    call c4ct_sl ( a(1,i3), x(1,1,i3), r, m, l, m2 )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c4salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c4cg_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C4CG_SL solves the C4 CG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M*M,L).
!    On input, the first row of blocks of the CG matrix.
!    Each block is represented by columns.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(M,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,l)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) ii
  complex ( kind = 4 ) r(*)
  complex ( kind = 4 ) x(m,l)
!
!  Reduce the CG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, m * m, l, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c4salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are G matrices.
!
  do i2 = 1, l
    call c4gefa ( a(1,i2), m, m, r, ii )
    call c4gesl ( a(1,i2), m, m, r, x(1,i2), 0 )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c4salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 4 )

  return
end
subroutine c4ci_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! C4CI_MXV multiplies a C4 circulant matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 4 ) A(N), the entries of the first row
!    of the circulant matrix.
!
!    Input, complex ( kind = 4 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 4 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  complex ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 4 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

    do j = 1, i - 1
      b(i) = b(i) + a(n+j+1-i) * x(j)
    end do

    do j = i, n
      b(i) = b(i) + a(j+1-i) * x(j)
    end do

  end do

  return
end
subroutine c4ci_print ( n, a, title )

!*****************************************************************************80
!
!! C4CI_PRINT prints a C4 circulant matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 4 ) A(N), the N by N circulant matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  character ( len = * ) title

  call c4ci_print_some ( n, a, 1, 1, n, n, title )

  return
end
subroutine c4ci_print_some ( n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! C4CI_PRINT_SOME prints some of a C4 circulant matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 4 ) A(N), the N by N circulant matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 4
  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  complex ( kind = 4 ) aij
  character ( len = 20 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
!
!  Print the columns of the matrix, in strips of 5.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i10,10x)' ) j
    end do

    write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) INCX entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+j+1-i)
        end if

        write ( ctemp(j2), '(2g10.3)' ) aij

      end do

      write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
subroutine c4ci_random ( n, seed, a )

!*****************************************************************************80
!
!! C4CI_RANDOM randomizes a C4 circulant matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 4 ) A(N), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  integer ( kind = 4 ) seed

  call c4vec_uniform_01 ( n, seed, a )

  return
end
subroutine c4ci_sl ( m, a, x )

!*****************************************************************************80
!
!! C4CI_SL solves the C4 circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix A.
!
!    Input, complex ( kind = 4 ) A(M), the first row of the circulant matrix.
!
!    Input/output, complex ( kind = 4 ) X(M)
!    On input, the right hand side vector.
!    On output, the solution vector.
!
  implicit none

  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(m)
  complex ( kind = 4 ) e
  complex ( kind = 4 ) e1
  complex ( kind = 4 ) f
  complex ( kind = 4 ) f1
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  real ( kind = 4 ) p
  complex ( kind = 4 ) r(m)
  real ( kind = 4 ) ri
  real ( kind = 4 ) rm
  complex ( kind = 4 ) t
  complex ( kind = 4 ) t1
  real ( kind = 4 ) v1
  real ( kind = 4 ) v2
  complex ( kind = 4 ) x(m)

  t1 = x(1)
  x(1) = t1 / a(1)

  if ( m == 1 ) then
    return
  end if

  rm = real ( m, kind = 4 )
!
!  Compute the inverse discrete Fourier transformation
!  of the first row of the matrix and the discrete
!  Fourier transformation of the right hand side vector.
!
  t = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

  do i1 = 1, m

    ri = real ( i1 - 1, kind = 4 )
!
!  Minimize error in forming multiples of 2 PI.
!
    p = ( ( 201.0E+00 / 32.0E+00 ) * ri &
      + 1.93530717958647692528E-03 * ri ) / real ( m, kind = 4 )

    v1 = cos ( p )
    v2 = sin ( p )
    e = cmplx ( v1, -v2, kind = 4 )
    e1 = cmplx ( v1, v2, kind = 4 )
    f = a(1)
    f1 = t1
    do i2 = 2, m
      f = e * f + a(i2)
      f1 = e1 * f1 + x(i2)
    end do
    r(i1) = ( e1 * f1 ) / ( e * f )
    t = t + r(i1)

  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  x(1) = t / rm

  do i1 = 2, m

    ri = real ( i1 - 1, kind = 4 )
!
!  Minimize the error in forming multiples of 2 PI.
!
    p = ( ( 201.0E+00 / 32.0E+00 ) * ri &
      + 1.93530717958647692528E-03 * ri ) / real ( m, kind = 4 )

    v1 = cos ( p )
    v2 = sin ( p )
    e = cmplx ( v1, -v2, kind = 4 )

    f = r(1)
    do i2 = 2, m
      f = e * f + r(i2)
    end do

    x(i1) = e * f / real ( m, kind = 4 )

  end do

  return
end
subroutine c4ctg_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C4CTG_SL solves the C4 CTG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M*M*(2*L - 1),K).
!    On input, the first row of outer blocks of the CTG matrix.
!    Each outer block is represented by its first row of inner blocks
!    followed by its first column of inner blocks beginning with the
!    second block.  Each inner block is represented by columns.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(M*M*(2*L + 3) + M,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mm
  complex ( kind = 4 ) r(*)
  real ( kind = 4 ) rk
  complex ( kind = 4 ) x(m,l,k)

  rk = real ( k, kind = 4 )
  mm = m * m
  ml = m * l
!
!  Reduce the CTG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c4salw ( a, mm*(2*l - 1), k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c4salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are block Toeplitz matrices.
!
  do i3 = 1, k
    call c4tg_sl ( a(1,i3), x(1,1,i3), r, m, l, lda )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c4salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c4ct_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C4CT_SL solves the C4 CT linear system A * X = B.
!
!  Discussion:
!
!    This routine can handle linear systems in which the matrix is
!    a complex CT matrix.  The entries are complex numbers.  The matrix
!    has a block structure.
!
!    The matrix has order L*M by L*M.
!    As a block matrix, the matrix has order L by L.
!    Each block has order M by M.
!
!    Each block is a Toeplitz matrix.
!    The blocks appear in the matrix in circulant form.
!
!    In other words, the block matrix is a circulant.  The individual
!    "entries" of the block matrix are Toeplitz matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(2*M-1,L).
!    On input, the first row of blocks of the CT matrix.
!    Each block is represented by its first row followed by its first
!    column beginning with the second element.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 4 ) X(M*L).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 4 ) R(max(2*M - 2,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,l)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) job
  complex ( kind = 4 ) r(*)
  complex ( kind = 4 ) x(m,l)
!
!  Reduce the CT matrix to a block-diagonal matrix by the inverse discrete
!  Fourier transform.
!
  call c4salw ( a, 2*m - 1, l, lda, -1 )
!
!  Compute the discrete Fourier transform of the right hand side.
!
  call c4salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are Toeplitz matrices.
!
  job = 0
  do i2 = 1, l
    call c4to_sl ( m, a(1,i2), x(1,i2), r, job )
  end do
!
!  Compute the solution of the given system by the inverse discrete
!  Fourier transform.
!
  call c4salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 4 )

  return
end
subroutine c4gefa ( a, lda, n, ipvt, info )

!*****************************************************************************80
!
!! C4GEFA factors a C4 matrix by gaussian elimination.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(LDA,N).
!    On input, the matrix to be factored.
!    On output, an upper triangular matrix and the multipliers used to
!    obtain it.
!    The factorization can be written A = L * U where L is a product of
!    permutation and unit lower triangular matrices and U is upper triangular.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Output, integer ( kind = 4 ) IPVT(N), the pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity indicator.
!    0, normal value.
!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
!    but it does indicate that C4GESL or C4GEDI will divide by zero
!    if called.  Use RCOND in C4GECO for a reliable indication of singularity.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) c4_abs1
  integer ( kind = 4 ) c4vec_amax_index
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  complex ( kind = 4 ) t
!
!  Gaussian elimination with partial pivoting.
!
  info = 0

  do k = 1, n - 1
!
!  Find L = pivot index.
!
    l = c4vec_amax_index ( n-k+1, a(k,k), 1 ) + k - 1
    ipvt(k) = l
!
!  A zero pivot implies this column already triangularized.
!
    if ( c4_abs1 ( a(l,k) ) == 0.0E+00 ) then
      info = k
      cycle
    end if
!
!  Interchange if necessary.
!
    if ( l /= k ) then
      call c4_swap ( a(l,k), a(k,k) )
    end if
!
!  Compute multipliers.
!
    t = - cmplx ( 1.0E+00, 0.0E+00, kind = 4 ) / a(k,k)
    call c4vec_scal ( n-k, t, a(k+1,k), 1 )
!
!  Row elimination with column indexing.
!
    do j = k + 1, n

      if ( l /= k ) then
        call c4_swap ( a(l,j), a(k,j) )
      end if

      t = a(k,j)
      call c4vec_axpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )

    end do

  end do

  ipvt(n) = n

  if ( c4_abs1 ( a(n,n) ) == 0.0E+00 ) then
    info = n
  end if

  return
end
subroutine c4gesl ( a, lda, n, ipvt, b, job )

!*****************************************************************************80
!
!! C4GESL solves the C4 general system A * X = B.
!
!  Discussion:
!
!    The system matrix must have been factored by C4GECO or C4GEFA.
!
!    The routine can also solve the system (A*) * X = B.
!
!    A division by zero will occur if the input factor contains a
!    zero on the diagonal.  Technically this indicates singularity
!    but it is often caused by improper arguments or improper
!    setting of LDA.  It will not occur if the subroutines are
!    called correctly and if C4GECO has set 0.0 < RCOND
!    or C4GEFA has set INFO == 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input, complex ( kind = 4 ) A(LDA,N), the output from C4GECO or C4GEFA.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) IPVT(N), the pivot vector from C4GECO
!    or C4GEFA.
!
!    Input/output, complex ( kind = 4 ) B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer ( kind = 4 ) JOB, specifies the task.
!    0, solve A * X = B,
!    nonzero, solve (A*) * X = B, where (A*) is the conjugate transpose.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(lda,n)
  complex ( kind = 4 ) b(n)
  complex ( kind = 4 ) c4vec_dotc
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  complex t
!
!  Solve A * X = B.
!
  if ( job ==  0 ) then

    do k = 1, n - 1

      l = ipvt(k)

      if ( l /= k ) then
        call c4_swap ( b(l), b(k) )
      end if

      t = b(k)
      call c4vec_axpy ( n-k, t, a(k+1,k), 1, b(k+1), 1 )

    end do

    do k = n, 1, -1
      b(k) = b(k) / a(k,k)
      t = -b(k)
      call c4vec_axpy ( k-1, t, a(1,k), 1, b(1), 1 )
    end do
!
!  Solve (A*) * X = B.
!
  else

    do k = 1, n
      t = c4vec_dotc ( k-1, a(1,k), 1, b(1), 1 )
      b(k) = ( b(k) - t ) / conjg ( a(k,k) )
    end do

    do k = n - 1, 1, -1

      b(k) = b(k) + c4vec_dotc ( n-k, a(k+1,k), 1, b(k+1), 1 )
      l = ipvt(k)

      if ( l /=  k ) then
        call c4_swap ( b(l), b(k) )
      end if

    end do

  end if

  return
end
subroutine c4salw ( a, m, l, lda, job )

!*****************************************************************************80
!
!! C4SALW Fourier transforms the rows of a C4 rectangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) A(M,L)
!    On input, the matrix to be transformed.
!    On output, the transformed matrix.
!
!    Input, integer ( kind = 4 ) M, L, the number of rows and columns of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) JOB.
!    +1, for direct Fourier transform.
!    -1, for inverse Fourier transform.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda

  complex ( kind = 4 ) a(lda,l)
  complex ( kind = 4 ) e
  complex ( kind = 4 ) f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) job
  integer ( kind = 4 ) m
  real ( kind = 4 ) p
  complex ( kind = 4 ) r1(l)
  complex ( kind = 4 ) r2(l)
  real ( kind = 4 ) ri
  real ( kind = 4 ) v1
  real ( kind = 4 ) v2

  if ( l <= 1 ) then
    return
  end if

  r1(1) = cmplx ( 1.0E+00, 0.0E+00, kind = 4 )

  do i1 = 2, l

    ri = real ( i1 - 1, kind = 4 )
!
!  Minimize error in forming multiples of 2 PI.
!
    p = ( ( 201.0E+00 / 32.0E+00 ) * ri &
      + 1.93530717958647692528E-03 * ri ) / real ( l, kind = 4 )

    v1 = cos ( p )
    v2 = sin ( p )
    if ( job == -1 ) then
      v2 = -v2
    end if

    r1(i1) = cmplx ( v1, v2, kind = 4 )

  end do

  do i = 1, m

    do i1 = 1, l
      e = r1(i1)
      f = a(i,1)
      do i2 = 2, l
        f = e * f + a(i,i2)
      end do
      r2(i1) = e * f
    end do

    a(i,1:l) = r2(1:l)

  end do

  return
end
subroutine c4_swap ( x, y )

!*****************************************************************************80
!
!! C4_SWAP swaps two C4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  complex ( kind = 4 ) x
  complex ( kind = 4 ) y
  complex ( kind = 4 ) z

  z = x
  x = y
  y = z

  return
end
subroutine c4tg_sl1 ( a1, a2, b, x, c1, c2, r1, r2, r3, r5, r6, m, &
  l, lda )

!*****************************************************************************80
!
!! C4TG_SL1 solves a C4 TG linear system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, complex ( kind = 4 ) A1(M*M,L), the first row of blocks of the
!    TG matrix.  Each block is represented by columns.
!
!    Input, complex ( kind = 4 ) A2(M*M,L-1), the first column of blocks of the
!    TG matrix, beginning with the second block.  Each block is
!    represented by columns.
!
!    Input, complex ( kind = 4 ) B(M*L), the right hand side vector.
!
!    Output, complex ( kind = 4 ) X(M*L), the solution vector, which may
!    coincide with B.
!
!    Workspace, complex ( kind = 4 ) C1(M,M,L-1), C2(M,M,L-1), R1(M,M), R2(M,M),
!    R3(M,M), R5(M,M), R6(M,M).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a1(lda,l)
  complex ( kind = 4 ) a2(lda,l-1)
  complex ( kind = 4 ) b(m,l)
  complex ( kind = 4 ) c1(m,m,l-1)
  complex ( kind = 4 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) pivot(m)
  complex ( kind = 4 ) r1(m,m)
  complex ( kind = 4 ) r2(m,m)
  complex ( kind = 4 ) r3(m,m)
  complex ( kind = 4 ) r5(m,m)
  complex ( kind = 4 ) r6(m,m)
  complex ( kind = 4 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1

  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      r3(i,j) = r1(i,j)
      i3 = i3 + 1
    end do
    x(j,1) = b(j,1)
  end do

  call c4gefa ( r3, m, m, pivot, ii )

  call c4gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system
!  with the TG matrix for N = 2 through L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    n1 = n - 1
    n2 = n - 2
    i3 = 1

    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      do j = 1, m
        do i = 1, m
          c1(i,j,n1) = r2(i,j)
        end do
      end do

      do i1 = 1, n2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            do ii = 1, m
              r5(ii,j) = r5(ii,j) + c1(i,j,i2)*a2(i3,i1)
              r6(ii,j) = r6(ii,j) + c2(i,j,i1)*a1(i3,i1+1)
              i3 = i3 + 1
            end do
          end do
        end do
      end do

    end if

    do j = 1, m
      do i = 1, m
        r2(i,j) = -r5(i,j)
      end do
      call c4gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    do j = 1, m
      do i = 1, m
        r3(i,j) = r6(i,j)
        r6(i,j) = -c1(i,j,1)
      end do
    end do

    do j = 1, m
      do i = 1, m
        do ii = 1, m
          c1(ii,j,1) = c1(ii,j,1) + r2(i,j)*r3(ii,i)
        end do
      end do
    end do

    call c4gefa ( r6, m, m, pivot, ii )

    do j = 1, m
      call c4gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        do ii = 1, m
          r1(ii,j) = r1(ii,j) + r3(i,j)*r5(ii,i)
        end do
      end do
    end do

    if ( 2 < n ) then

      do j = 1, m
        do i = 1, m
          r6(i,j) = c2(i,j,1)
        end do
      end do

      do i1 = 2, n1

        if ( i1 /= n1 ) then
          do j = 1, m
            do i = 1, m
              r5(i,j) = c2(i,j,i1)
            end do
          end do
        end if

        do j = 1, m
          do i = 1, m
            c2(i,j,i1) = r6(i,j)
          end do
          do i = 1, m
            do ii = 1, m
              c2(ii,j,i1) = c2(ii,j,i1) + r3(i,j)*c1(ii,i,i1)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, m
            do ii = 1, m
              c1(ii,j,i1) = c1(ii,j,i1) + r2(i,j)*r6(ii,i)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, m
            r6(i,j) = r5(i,j)
          end do
        end do

      end do

    end if

    do j = 1, m
      do i = 1, m
        c2(i,j,1) = r3(i,j)
      end do
    end do
!
!  Compute the solution of the system with the
!  principal minor of order M*N.
!
    do j = 1, m
      do i = 1, m
        r3(i,j) = r1(i,j)
      end do
      x(j,n) = b(j,n)
    end do

    do i1 = 1, n1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        do ii = 1, m
          x(ii,n) = x(ii,n) - x(i,i2)*a2(i3,i1)
          i3 = i3 + 1
        end do
      end do
    end do

    call c4gefa ( r3, m, m, pivot, ii )
    call c4gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n1
      do i = 1, m
        do ii = 1, m
          x(ii,i1) = x(ii,i1) + x(i,n)*c2(ii,i,i1)
        end do
      end do
    end do

  end do

  return
end
subroutine c4tg_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C4TG_SL solves a linear system involving a C4 TG matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, complex ( kind = 4 ) A(LDA,2*L-1), an M*M by 2*L-2 array, containing
!    the first row of blocks of the CTG matrix, followed by its first
!    column of blocks beginning with the second block.  Each block is
!    represented by columns.
!
!    Input/output, complex ( kind = 4 ) A(M*L).  On input, the right hand side
!    vector, and on output the solution.
!
!    Workspace, complex ( kind = 4 ) R(M*M*(2*L+3)+M).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A, which must
!    be at least M*M.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 4 ) a(lda,2*l-1)
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) mml
  integer ( kind = 4 ) mml1
  integer ( kind = 4 ) mml2
  integer ( kind = 4 ) mml3
  integer ( kind = 4 ) mml4
  integer ( kind = 4 ) mml5
  complex ( kind = 4 ) r(*)
  complex ( kind = 4 ) x(m,l)

  mm = m * m
  mml = mm*(l - 1) + 1
  mml1 = 2*mml - 1
  mml2 = mml1 + mm
  mml3 = mml2 + mm
  mml4 = mml3 + mm
  mml5 = mml4 + mm

  call c4tg_sl1 ( a, a(1,l+1), x, x, r, r(mml), r(mml1), r(mml2), &
    r(mml3), r(mml4), r(mml5), m, l, lda )

  return
end
subroutine c4to_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! C4TO_MXV multiplies a C4 Toeplitz matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 4 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, complex ( kind = 4 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 4 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  complex ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 4 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

    do j = 1, i - 1
      b(i) = b(i) + a(n+i-j) * x(j)
    end do

    do j = i, n
      b(i) = b(i) + a(j+1-i) * x(j)
    end do

  end do

  return
end
subroutine c4to_print ( n, a, title )

!*****************************************************************************80
!
!! C4TO_PRINT prints a C4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 4 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  character ( len = * ) title

  call c4to_print_some ( n, a, 1, 1, n, n, title )

  return
end
subroutine c4to_print_some ( n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! C4TO_PRINT_SOME prints some of a C4 Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 4 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 4
  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  complex ( kind = 4 ) aij
  character ( len = 20 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
!
!  Print the columns of the matrix, in strips of INCX.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i10,10x)' ) j
    end do

    write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )

    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) INCX entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+i-j)
        end if

        if ( aij == cmplx ( 0.0E+00, 0.0E+00, kind = 4 ) ) then
          ctemp(j2) = '    0.0'
        else if ( aimag ( aij ) == 0.0E+00 ) then
          write ( ctemp(j2), '(g10.3,10x)' ) real ( aij )
        else
          write ( ctemp(j2), '(2g10.3)' ) aij
        end if

      end do

      write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
subroutine c4to_random ( n, seed, a )

!*****************************************************************************80
!
!! C4TO_RANDOM randomizes a C4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 4 ) A(2*N-1), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  integer ( kind = 4 ) seed

  call c4vec_uniform_01 ( 2 * n - 1, seed, a )

  return
end
subroutine c4to_sl ( n, a, b, x, job )

!*****************************************************************************80
!
!! C4TO_SL solves the C4 Toeplitz system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 4 ) A(2*N-1), the first row of the Toeplitz matrix,
!    followed by the first column of the Toeplitz matrix, beginning with the
!    second element.
!
!    Input, complex ( kind = 4 ) B(N) the right hand side vector.
!
!    Output, complex ( kind = 4 ) X(N), the solution vector.  X and B may share
!    the same storage.
!
!    Input, integer ( kind = 4 ) JOB,
!    0 to solve A*X=B,
!    nonzero to solve A'*X=B.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  complex ( kind = 4 ) b(n)
  complex ( kind = 4 ) c1(n-1)
  complex ( kind = 4 ) c2(n-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) job
  integer ( kind = 4 ) nsub
  complex ( kind = 4 ) r1
  complex ( kind = 4 ) r2
  complex ( kind = 4 ) r3
  complex ( kind = 4 ) r5
  complex ( kind = 4 ) r6
  complex ( kind = 4 ) x(n)

  if ( n < 1 ) then
    return
  end if
!
!  Solve the system with the principal minor of order 1.
!
  r1 = a(1)
  x(1) = b(1) / r1

  if ( n == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the Toeplitz matrix.
!
  do nsub = 2, n
!
!  Compute multiples of the first and last columns of the inverse of
!  the principal minor of order NSUB.
!
    if ( job == 0 ) then
      r5 = a(n+nsub-1)
      r6 = a(nsub)
    else
      r5 = a(nsub)
      r6 = a(n+nsub-1)
    end if

    if ( 2 < nsub ) then

      c1(nsub-1) = r2

      do i = 1, nsub-2
        if ( job == 0 ) then
          r5 = r5 + a(n+i) * c1(nsub-i)
          r6 = r6 + a(i+1) * c2(i)
        else
          r5 = r5 + a(i+1) * c1(nsub-i)
          r6 = r6 + a(n+i) * c2(i)
        end if
      end do

    end if

    r2 = -r5 / r1
    r3 = -r6 / r1
    r1 = r1 + r5 * r3

    if ( 2 < nsub ) then

      r6 = c2(1)
      c2(nsub-1) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )
      do i = 2, nsub - 1
        r5 = c2(i)
        c2(i) = c1(i) * r3 + r6
        c1(i) = c1(i) + r6 * r2
        r6 = r5
      end do

    end if

    c2(1) = r3
!
!  Compute the solution of the system with the principal minor of order NSUB.
!
    r5 = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

    do i = 1, nsub - 1
      if ( job == 0 ) then
        r5 = r5 + a(n+i) * x(nsub-i)
      else
        r5 = r5 + a(i+1) * x(nsub-i)
      end if
    end do

    r6 = ( b(nsub) - r5 ) / r1

    x(1:nsub-1) = x(1:nsub-1) + c2(1:nsub-1) * r6
    x(nsub) = r6

  end do

  return
end
subroutine c4to_vxm ( n, a, x, b )

!*****************************************************************************80
!
!! C4TO_VXM multiplies a vector times a C4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 4 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, complex ( kind = 4 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 4 ) B(N), the product A' * X.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(2*n-1)
  complex ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 4 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

    do j = 1, i
      b(i) = b(i) + a(i+1-j) * x(j)
    end do

    do j = i+1, n
      b(i) = b(i) + a(n+j-i) * x(j)
    end do

  end do

  return
end
subroutine c4trdi ( t, ldt, n, det, job, info )

!*****************************************************************************80
!
!! C4TRDI computes the determinant and inverse of a C4 triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, complex ( kind = 4 ) T(LDT,N).
!    On input, T contains the triangular matrix.  The zero elements of the
!    matrix are not referenced, and the corresponding elements of the array
!    can be used to store other information.
!    On output, T contains the inverse of the matrix, if requested.
!
!    Input, integer ( kind = 4 ) LDT, the leading dimension of T.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix T.
!
!    Output, complex ( kind = 4 ) DET(2), the determinant if requested.
!    The determinant = DET(1) * 10.0**DET(2)
!    with 1.0 <= c4_abs1(DET(1)) < 10.0 or DET(1) == 0.0.
!
!    Input, integer ( kind = 4 ) JOB, indicates the shape of the matrix,
!    and the task.
!    010, inverse of lower triangular matrix only.
!    011, inverse of upper triangular matrix only.
!    100, determinant only.
!    110, determinant and inverse of lower triangular matrix.
!    111, determinant and inverse of upper triangular matrix.
!
!    Output, integer ( kind = 4 ) INFO, inverse information.
!    If the inverse was requested, then INFO is:
!    0, if the system is nonsingular,
!    nonzero, if the system is singular.
!
  implicit none

  integer ( kind = 4 ) ldt
  integer ( kind = 4 ) n

  real ( kind = 4 ) c4_abs1
  complex ( kind = 4 ) det(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  complex ( kind = 4 ) t(ldt,n)
  complex ( kind = 4 ) temp
  real ( kind = 4 ), parameter :: ten = 10.0E+00
!
!  Determinant
!
  if ( job / 100 /= 0 ) then

    det(1) = cmplx ( 1.0E+00, 0.0E+00, kind = 4 )
    det(2) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

    do i = 1, n

      det(1) = t(i,i) * det(1)

      if ( c4_abs1 ( det(1) ) == 0.0E+00 ) then
        exit
      end if

      do while ( c4_abs1 ( det(1) ) < 1.0E+00 )
        det(1) = cmplx ( ten, 0.0E+00, kind = 4 ) * det(1)
        det(2) = det(2) - cmplx ( 1.0E+00, 0.0E+00, kind = 4 )
      end do

      do while ( ten <= c4_abs1 ( det(1) ) )
        det(1) = det(1) / cmplx ( ten, 0.0E+00, kind = 4 )
        det(2) = det(2) + cmplx ( 1.0E+00, 0.0E+00, kind = 4 )
      end do

    end do

  end if

  if ( mod ( job / 10, 10 ) == 0 ) then
    return
  end if
!
!  Inverse of upper triangular matrix.
!
  if ( mod ( job, 10 ) /= 0 ) then

    info = 0

    do k = 1, n

      if ( c4_abs1 ( t(k,k) ) == 0.0E+00 ) then
        info = k
        exit
      end if

      t(k,k) = cmplx ( 1.0E+00, 0.0E+00, kind = 4 ) / t(k,k)
      temp = -t(k,k)
      call c4vec_scal ( k - 1, temp, t(1,k), 1 )

      do j = k + 1, n
        temp = t(k,j)
        t(k,j) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )
        call c4vec_axpy ( k, temp, t(1,k), 1, t(1,j), 1 )
      end do

    end do

  else
!
!  Inverse of lower triangular matrix.
!
    info = 0

    do k = n, 1, -1

      if ( c4_abs1 ( t(k,k) ) == 0.0E+00 ) then
        info = k
        exit
      end if

      t(k,k) = cmplx ( 1.0E+00, 0.0E+00, kind = 4 ) / t(k,k)
      temp = -t(k,k)

      if ( k /= n ) then
        call c4vec_scal ( n-k, temp, t(k+1,k), 1 )
      end if

      do j = 1, k - 1
        temp = t(k,j)
        t(k,j) = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )
        call c4vec_axpy ( n-k+1, temp, t(k,k), 1, t(k,j), 1 )
      end do

    end do

  end if

  return
end
function c4vec_amax_index ( n, cx, incx )

!*****************************************************************************80
!
!! C4VEC_AMAX_INDEX indexes the C4VEC element of maximum absolute value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 4 ) C(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
!    Output, integer ( kind = 4 ) C4VEC_AMAX_INDEX, the index of the element
!    of CX of maximum absolute value.
!
  implicit none

  real ( kind = 4 ) c4_abs1
  integer ( kind = 4 ) c4vec_amax_index
  complex ( kind = 4 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) smax

  if ( n < 1 ) then
    c4vec_amax_index = 0
    return
  end if

  c4vec_amax_index = 1

  if ( n == 1 ) then
    return
  end if

  if ( incx == 1 ) then

    smax = c4_abs1 ( cx(1) )

    do i = 2, n
      if ( smax < c4_abs1 ( cx(i) ) ) then
        c4vec_amax_index = i
        smax = c4_abs1 ( cx(i) )
      end if
    end do

  else

    ix = 1
    smax = c4_abs1 ( cx(1) )
    ix = ix + incx
    do i = 2, n
      if ( smax < c4_abs1 ( cx(ix) ) ) then
        c4vec_amax_index = i
        smax = c4_abs1 ( cx(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
subroutine c4vec_axpy ( n, ca, cx, incx, cy, incy )

!*****************************************************************************80
!
!! C4VEC_AXPY adds a constant times one C4VEC to another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 4 ) CA, the multiplier.
!
!    Input, complex ( kind = 4 ) CX(*), the vector to be scaled and added to Y.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input/output, complex ( kind = 4 ) Y(*), the vector to which a multiple of
!    X is to be added.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries of Y.
!
  implicit none

  complex ( kind = 4 ) ca
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  if ( n <= 0 ) then

  else if ( ca == cmplx ( 0.0E+00, 0.0E+00, kind = 4 ) ) then

  else if ( incx == 1 .and. incy == 1 ) then

    cy(1:n) = cy(1:n) + ca * cx(1:n)

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      cy(iy) = cy(iy) + ca * cx(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
function c4vec_dotc ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! C4VEC_DOTC forms the dot product of two C4VEC's, conjugating the first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, complex ( kind = 4 ) CX(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
!    Input, complex ( kind = 4 ) CY(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of CY.
!
!    Output, real ( kind = 4 ) C4VEC_DOTC, the (conjugated) dot product
!    of CX and CY.
!
  implicit none

  complex ( kind = 4 ) c4vec_dotc
  complex ( kind = 4 ) ctemp
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  ctemp = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

  c4vec_dotc = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    ctemp = dot_product ( conjg ( cx(1:n) ), cy(1:n) )

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp = ctemp + conjg ( cx(ix) ) * cy(iy)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  c4vec_dotc = ctemp

  return
end
subroutine c4vec_indicator ( n, a )

!*****************************************************************************80
!
!! C4VEC_INDICATOR sets a C4VEC to the indicator vector.
!
!  Discussion:
!
!    X(1:N) = ( 1-1i, 2-2i, 3-3i, 4-4i, ... )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, complex ( kind = 4 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = cmplx ( i, -i, kind = 4 )
  end do

  return
end
function c4vec_nrm2 ( n, x, incx )

!*****************************************************************************80
!
!! C4VEC_NRM2 computes the unitary norm of a C4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 4 ) X(*), the vector whose norm is to be computed.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 4 ) C4VEC_NRM2, the unitary norm of X.
!
  implicit none

  real ( kind = 4 ) c4vec_nrm2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) stemp
  complex ( kind = 4 ) x(*)

  if ( n <= 0 ) then
    c4vec_nrm2 = 0.0E+00
    return
  end if

  if ( 0 <= incx ) then
    ix = 1
  else
    ix = ( - n + 1 ) * incx + 1
  end if

  stemp = 0.0E+00

  do i = 1, n
    stemp = stemp + conjg ( x(ix) ) * x(ix)
    ix = ix + incx
  end do

  c4vec_nrm2 = sqrt ( stemp )

  return
end
subroutine c4vec_print ( n, a, title )

!*****************************************************************************80
!
!! C4VEC_PRINT prints a C4VEC, with an optional title.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, complex ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(i8,2g14.6)' ) i, a(i)
  end do

  return
end
subroutine c4vec_print_some ( n, x, max_print )

!*****************************************************************************80
!
!! C4VEC_PRINT_SOME prints some of a C4VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, complex ( kind = 4 ) X(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  complex ( kind = 4 ) x(n)

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,2g14.6)' ) i, x(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    i = max_print
    write ( *, '(i8,2x,2g14.6,2x,a)' ) i, x(i), '...more entries...'

  end if

  return
end
subroutine c4vec_scal ( n, ca, cx, incx )

!*****************************************************************************80
!
!! C4VEC_SCAL scales a C4VEC by a constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 4 ) CA, the multiplier.
!
!    Input/output, complex ( kind = 4 ) CX(*), the vector to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
  implicit none

  complex ( kind = 4 ) ca
  complex ( kind = 4 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    cx(1:n) = ca * cx(1:n)

  else

    do i = 1, n * incx, incx
      cx(i) = ca * cx(i)
    end do

  end if

  return
end
subroutine c4vec_scal_r4 ( n, sa, cx, incx )

!*****************************************************************************80
!
!! C4VEC_SCAL_R4 scales a C4VEC by an R4 constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) SA, the multiplier.
!
!    Input/output, complex ( kind = 4 ) CX(N), the vector to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
  implicit none

  complex ( kind = 4 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  real ( kind = 4 ) sa

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    do i = 1, n
      cx(i) = cmplx ( sa * real ( cx(i) ), sa * aimag ( cx(i) ), kind = 4 )
    end do

  else

    nincx = n * incx
    do i = 1, nincx, incx
      cx(i) = cmplx ( sa * real ( cx(i) ), sa * aimag ( cx(i) ), kind = 4 )
    end do

  end if

  return
end
subroutine c4vec_uniform_01 ( n, seed, c )

!*****************************************************************************80
!
!! C4VEC_UNIFORM_01 returns a unit pseudorandom C4VEC.
!
!  Discussion:
!
!    A C4VEC is a vector of C4's.
!
!    The angles should be uniformly distributed between 0 and 2 * PI,
!    the square roots of the radius uniformly distributed between 0 and 1.
!
!    This results in a uniform distribution of values in the unit circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values to compute.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, complex ( kind = 4 ) C(N), the pseudorandom complex vector.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  real ( kind = 4 ), parameter :: r4_pi = 3.141592653589793E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) theta

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r = sqrt ( real ( seed, kind = 4 ) * 4.656612875E-10 )

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    theta = 2.0E+00 * r4_pi * ( real ( seed, kind = 4 ) * 4.656612875E-10 )

    c(i) = r * cmplx ( cos ( theta ), sin ( theta ), kind = 4 )

  end do

  return
end
function c8_abs1 ( x )

!*****************************************************************************80
!
!! C8_ABS1 computes the L1 absolute value of a C8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) X, the number whose L1 absolute value
!    is desired.
!
!    Output, real ( kind = 8 ) C8_ABS1, the L1 absolute value of X.
!
  implicit none

  real ( kind = 8 ) c8_abs1
  complex ( kind = 8 ) x

  c8_abs1 = abs ( real ( x ) ) + abs ( imag ( x ) )

  return
end
subroutine c8bto_sl ( m, l, a1, a2, b, x )

!*****************************************************************************80
!
!! C8BTO_SL solves the C8 block Toeplitz linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, complex ( kind = 8 ) A1(M*M,L), the first row of blocks of the
!    matrix.  Each block is represented by columns.
!
!    Input, complex ( kind = 8 ) A2(M*M,L-1), the first column of blocks of
!    the matrix, beginning with the second block.  Each block is represented
!    by columns.
!
!    Input, complex ( kind = 8 ) B(M*L), the right hand side vector.
!
!    Output, complex ( kind = 8 ) X(M*L), the solution vector.  X may coincide
!    with B.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a1(m*m,l)
  complex ( kind = 8 ) a2(m*m,1)
  complex ( kind = 8 ) b(m,l)
  complex ( kind = 8 ) c1(m,m,l-1)
  complex ( kind = 8 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) pivot(m)
  complex ( kind = 8 ) r1(m,m)
  complex ( kind = 8 ) r2(m,m)
  complex ( kind = 8 ) r3(m,m)
  complex ( kind = 8 ) r5(m,m)
  complex ( kind = 8 ) r6(m,m)
  complex ( kind = 8 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1
  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      r3(i,j) = r1(i,j)
      i3 = i3 + 1
    end do
    x(j,1) = b(j,1)
  end do

  call c8gefa ( r3, m, m, pivot, ii )

  call c8gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the block Toeplitz matrix
!  for N = 2, L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    i3 = 1
    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n-1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      c1(1:m,1:m,n-1) = r2(1:m,1:m)

      do i1 = 1, n - 2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            call c8vec_axpy ( m, c1(i,j,i2), a2(i3,i1), 1, r5(1,j), 1 )
            call c8vec_axpy ( m, c2(i,j,i1), a1(i3,i1+1), 1, r6(1,j), 1 )
            i3 = i3 + m
          end do
        end do
      end do

    end if

    do j = 1, m
      r2(1:m,j) = -r5(1:m,j)
      call c8gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    r3(1:m,1:m) = r6(1:m,1:m)
    r6(1:m,1:m) = -c1(1:m,1:m,1)

    do j = 1, m
      do i = 1, m
        call c8vec_axpy ( m, r2(i,j), r3(1,i), 1, c1(1,j,1), 1 )
      end do
    end do

    call c8gefa ( r6, m, m, pivot, ii )

    do j = 1, m
      call c8gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        call c8vec_axpy ( m, r3(i,j), r5(1,i), 1, r1(1,j), 1 )
      end do
    end do

    if ( 2 < n ) then

      r6(1:m,1:m) = c2(1:m,1:m,1)

      do i1 = 2, n - 1

        if ( i1 /= n - 1 ) then
          r5(1:m,1:m) = c2(1:m,1:m,i1)
        end if

        do j = 1, m
          c2(1:m,j,i1) = r6(1:m,j)
          do i = 1, m
            call c8vec_axpy ( m, r3(i,j), c1(1,i,i1), 1, c2(1,j,i1), 1 )
          end do
        end do

        do j = 1, m
          do i = 1, m
            call c8vec_axpy ( m, r2(i,j), r6(1,i), 1, c1(1,j,i1), 1 )
          end do
        end do

        r6(1:m,1:m) = r5(1:m,1:m)

      end do

    end if

    c2(1:m,1:m,1) = r3(1:m,1:m)
!
!  Compute the solution of the system with the principal minor of order M*N.
!
    r3(1:m,1:m) = r1(1:m,1:m)
    x(1:m,n) = b(1:m,n)

    do i1 = 1, n - 1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        call c8vec_axpy ( m, -x(i,i2), a2(i3,i1), 1, x(1,n), 1 )
        i3 = i3 + m
      end do
    end do

    call c8gefa ( r3, m, m, pivot, ii )

    call c8gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n - 1
      do i = 1, m
        call c8vec_axpy ( m, x(i,n), c2(1,i,i1), 1, x(1,i1), 1 )
      end do
    end do

  end do

  return
end
subroutine c8ccc_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C8CCC_SL solves the C8 double column circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M*L,K), the first row of outer blocks
!    of the CCC matrix.  Each outer block is represented by its first row
!    of inner blocks.  Each inner block is represented by its first row.
!    On return, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L*K)
!    On input, the right hand side vector.
!    On output, the solution.
!
!    Workspace, complex ( kind = 8 ) R(max(M,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  complex ( kind = 8 ) r(1)
  real ( kind = 8 ) rk
  complex ( kind = 8 ) x(m,l,k)

  rk = real ( k, kind = 8 )
  ml = m * l
!
!  Reduce the CCC matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, ml, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of the right hand side vector.
!
  call c8salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CC matrices.
!
  do i3 = 1, k
    call c8cc_sl ( a(1,i3), x(1,1,i3), r, m, l, m )
  end do
!
!  Solve the system by the inverse discrete Fourier transformation.
!
  call c8salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c8ccg_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C8CCG_SL solves the C8 CCG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M*M*L,K).
!    On input, the first row of outer blocks of the CCG matrix.
!    Each outer block is represented by its first row
!    of inner blocks.  Each inner block is represented by columns.
!    On return, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(M,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mm
  complex ( kind = 8 ) r(1)
  real ( kind = 8 ) rk
  complex ( kind = 8 ) x(m,l,k)

  rk = real ( k, kind = 8 )
  mm = m * m
  ml = m * l
!
!  Reduce the CCG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, mm * l, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c8salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CG matrices.
!
  do i3 = 1, k
    call c8cg_sl ( a(1,i3), x(1,1,i3), r, m, l, mm )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c8salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c8cc_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C8CC_SL solves the C8 column circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M,L)
!    On input, the first row of blocks of the CC matrix.
!    Each block is represented by its first row.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L)
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(M,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,l)
  integer ( kind = 4 ) i2
  complex ( kind = 8 ) r(*)
  complex ( kind = 8 ) x(m,l)
!
!  Reduce the CC matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, m, l, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c8salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are circulant matrices.
!
  do i2 = 1, l
    call c8ci_sl ( m, a(1,i2), x(1,i2) )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c8salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 8 )

  return
end
subroutine c8cct_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C8CCT_SL solves the C8 CCT linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A((2*M-1)*L,K).
!    On input, the first row of outer blocks of the CCT matrix.
!    Each outer block is represented by its first row of inner blocks.
!    Each inner block is represented by its first row followed by its
!    first column beginning with the second element.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(2*M - 2,2*L,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) ml
  complex ( kind = 8 ) r(*)
  real ( kind = 8 ) rk
  complex ( kind = 8 ) x(m,l,k)

  rk = real ( k, kind = 8 )
  m2 = 2 * m - 1
  ml = m * l
!
!  Reduce the CCT matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, m2 * l, k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c8salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are CT matrices.
!
  do i3 = 1, k
    call c8ct_sl ( a(1,i3), x(1,1,i3), r, m, l, m2 )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c8salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c8cg_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C8CG_SL solves the C8 CG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M*M,L).
!    On input, the first row of blocks of the CG matrix.
!    Each block is represented by columns.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(M,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,l)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) ii
  complex ( kind = 8 ) r(*)
  complex ( kind = 8 ) x(m,l)
!
!  Reduce the CG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, m * m, l, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c8salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are G matrices.
!
  do i2 = 1, l
    call c8gefa ( a(1,i2), m, m, r, ii )
    call c8gesl ( a(1,i2), m, m, r, x(1,i2), 0 )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c8salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 8 )

  return
end
subroutine c8ci_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! C8CI_MXV multiplies a C8 circulant matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 8 ) A(N), the entries of the first row
!    of the circulant matrix.
!
!    Input, complex ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 8 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  complex ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 8 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

    do j = 1, i - 1
      b(i) = b(i) + a(n+j+1-i) * x(j)
    end do

    do j = i, n
      b(i) = b(i) + a(j+1-i) * x(j)
    end do

  end do

  return
end
subroutine c8ci_print ( n, a, title )

!*****************************************************************************80
!
!! C8CI_PRINT prints a C8 circulant matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 8 ) A(N), the N by N circulant matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  character ( len = * ) title

  call c8ci_print_some ( n, a, 1, 1, n, n, title )

  return
end
subroutine c8ci_print_some ( n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! C8CI_PRINT_SOME prints some of a C8 circulant matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 8 ) A(N), the N by N circulant matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 4
  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  complex ( kind = 8 ) aij
  character ( len = 20 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
!
!  Print the columns of the matrix, in strips of 5.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i10,10x)' ) j
    end do

    write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) INCX entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+j+1-i)
        end if

        write ( ctemp(j2), '(2g10.3)' ) aij

      end do

      write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
subroutine c8ci_random ( n, seed, a )

!*****************************************************************************80
!
!! C8CI_RANDOM randomizes a C8 circulant matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 8 ) A(N), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) seed

  call c8vec_uniform_01 ( n, seed, a )

  return
end
subroutine c8ci_sl ( m, a, x )

!*****************************************************************************80
!
!! C8CI_SL solves the C8 circulant system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the matrix A.
!
!    Input, complex ( kind = 8 ) A(M), the first row of the circulant matrix.
!
!    Input/output, complex ( kind = 8 ) X(M)
!    On input, the right hand side vector.
!    On output, the solution vector.
!
  implicit none

  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(m)
  complex ( kind = 8 ) e
  complex ( kind = 8 ) e1
  complex ( kind = 8 ) f
  complex ( kind = 8 ) f1
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  real ( kind = 8 ) p
  complex ( kind = 8 ) r(m)
  real ( kind = 8 ) ri
  real ( kind = 8 ) rm
  complex ( kind = 8 ) t
  complex ( kind = 8 ) t1
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  complex ( kind = 8 ) x(m)

  t1 = x(1)
  x(1) = t1 / a(1)

  if ( m == 1 ) then
    return
  end if

  rm = real ( m, kind = 8 )
!
!  Compute the inverse discrete Fourier transformation
!  of the first row of the matrix and the discrete
!  Fourier transformation of the right hand side vector.
!
  t = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  do i1 = 1, m

    ri = real ( i1 - 1, kind = 8 )
!
!  Minimize error in forming multiples of 2 PI.
!
    p = ( ( 201.0D+00 / 32.0D+00 ) * ri &
      + 1.93530717958647692528D-03 * ri ) / real ( m, kind = 8 )

    v1 = cos ( p )
    v2 = sin ( p )
    e = cmplx ( v1, -v2, kind = 8 )
    e1 = cmplx ( v1, v2, kind = 8 )
    f = a(1)
    f1 = t1
    do i2 = 2, m
      f = e * f + a(i2)
      f1 = e1 * f1 + x(i2)
    end do
    r(i1) = ( e1 * f1 ) / ( e * f )
    t = t + r(i1)

  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  x(1) = t / rm

  do i1 = 2, m

    ri = real ( i1 - 1, kind = 8 )
!
!  Minimize the error in forming multiples of 2 PI.
!
    p = ( ( 201.0D+00 / 32.0D+00 ) * ri &
      + 1.93530717958647692528D-03 * ri ) / real ( m, kind = 8 )

    v1 = cos ( p )
    v2 = sin ( p )
    e = cmplx ( v1, -v2, kind = 8 )

    f = r(1)
    do i2 = 2, m
      f = e * f + r(i2)
    end do

    x(i1) = e * f / real ( m, kind = 8 )

  end do

  return
end
subroutine c8ctg_sl ( a, x, r, m, l, k, lda )

!*****************************************************************************80
!
!! C8CTG_SL solves the C8 CTG linear system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M*M*(2*L - 1),K).
!    On input, the first row of outer blocks of the CTG matrix.
!    Each outer block is represented by its first row of inner blocks
!    followed by its first column of inner blocks beginning with the
!    second block.  Each inner block is represented by columns.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L*K).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(M*M*(2*L + 3) + M,2*K)).
!
!    Input, integer ( kind = 4 ) M, the order of the inner blocks of the
!    matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of inner blocks in a row or
!    column of an outer block of A.
!
!    Input, integer ( kind = 4 ) K, the number of outer blocks in a row or
!    column of the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,k)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mm
  complex ( kind = 8 ) r(*)
  real ( kind = 8 ) rk
  complex ( kind = 8 ) x(m,l,k)

  rk = real ( k, kind = 8 )
  mm = m * m
  ml = m * l
!
!  Reduce the CTG matrix to a block-diagonal matrix
!  by the inverse discrete Fourier transformation.
!
  call c8salw ( a, mm*(2*l - 1), k, lda, -1 )
!
!  Compute the discrete Fourier transformation of
!  the right hand side vector.
!
  call c8salw ( x, ml, k, ml, 1 )
!
!  Solve the block-diagonal system, blocks of which are block Toeplitz matrices.
!
  do i3 = 1, k
    call c8tg_sl ( a(1,i3), x(1,1,i3), r, m, l, lda )
  end do
!
!  Compute the solution of the given system by
!  the inverse discrete Fourier transformation.
!
  call c8salw ( x, ml, k, ml, -1 )

  x(1:m,1:l,1:k) = x(1:m,1:l,1:k) / rk

  return
end
subroutine c8ct_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C8CT_SL solves the C8 CT linear system A * X = B.
!
!  Discussion:
!
!    This routine can handle linear systems in which the matrix is
!    a complex CT matrix.  The entries are complex numbers.  The matrix
!    has a block structure.
!
!    The matrix has order L*M by L*M.
!    As a block matrix, the matrix has order L by L.
!    Each block has order M by M.
!
!    Each block is a Toeplitz matrix.
!    The blocks appear in the matrix in circulant form.
!
!    In other words, the block matrix is a circulant.  The individual
!    "entries" of the block matrix are Toeplitz matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(2*M-1,L).
!    On input, the first row of blocks of the CT matrix.
!    Each block is represented by its first row followed by its first
!    column beginning with the second element.
!    On output, A has been destroyed.
!
!    Input/output, complex ( kind = 8 ) X(M*L).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Workspace, complex ( kind = 8 ) R(max(2*M - 2,2*L)).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,l)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) job
  complex ( kind = 8 ) r(*)
  complex ( kind = 8 ) x(m,l)
!
!  Reduce the CT matrix to a block-diagonal matrix by the inverse discrete
!  Fourier transform.
!
  call c8salw ( a, 2*m - 1, l, lda, -1 )
!
!  Compute the discrete Fourier transform of the right hand side.
!
  call c8salw ( x, m, l, m, 1 )
!
!  Solve the block-diagonal system, blocks of which are Toeplitz matrices.
!
  job = 0
  do i2 = 1, l
    call c8to_sl ( m, a(1,i2), x(1,i2), r, job )
  end do
!
!  Compute the solution of the given system by the inverse discrete
!  Fourier transform.
!
  call c8salw ( x, m, l, m, -1 )

  x(1:m,1:l) = x(1:m,1:l) / real ( l, kind = 8 )

  return
end
subroutine c8gefa ( a, lda, n, ipvt, info )

!*****************************************************************************80
!
!! C8GEFA factors a C8 matrix by gaussian elimination.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(LDA,N).
!    On input, the matrix to be factored.
!    On output, an upper triangular matrix and the multipliers used to
!    obtain it.
!    The factorization can be written A = L * U where L is a product of
!    permutation and unit lower triangular matrices and U is upper triangular.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Output, integer ( kind = 4 ) IPVT(N), the pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity indicator.
!    0, normal value.
!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
!    but it does indicate that C8GESL or C8GEDI will divide by zero
!    if called.  Use RCOND in C8GECO for a reliable indication of singularity.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) c8_abs1
  integer ( kind = 4 ) c8vec_amax_index
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  complex ( kind = 8 ) t
!
!  Gaussian elimination with partial pivoting.
!
  info = 0

  do k = 1, n - 1
!
!  Find L = pivot index.
!
    l = c8vec_amax_index ( n-k+1, a(k,k), 1 ) + k - 1
    ipvt(k) = l
!
!  A zero pivot implies this column already triangularized.
!
    if ( c8_abs1 ( a(l,k) ) == 0.0D+00 ) then
      info = k
      cycle
    end if
!
!  Interchange if necessary.
!
    if ( l /= k ) then
      call c8_swap ( a(l,k), a(k,k) )
    end if
!
!  Compute multipliers.
!
    t = - cmplx ( 1.0D+00, 0.0D+00, kind = 8 ) / a(k,k)
    call c8vec_scal ( n-k, t, a(k+1,k), 1 )
!
!  Row elimination with column indexing.
!
    do j = k + 1, n

      if ( l /= k ) then
        call c8_swap ( a(l,j), a(k,j) )
      end if

      t = a(k,j)
      call c8vec_axpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )

    end do

  end do

  ipvt(n) = n

  if ( c8_abs1 ( a(n,n) ) == 0.0D+00 ) then
    info = n
  end if

  return
end
subroutine c8gesl ( a, lda, n, ipvt, b, job )

!*****************************************************************************80
!
!! C8GESL solves the C8 general system A * X = B.
!
!  Discussion:
!
!    The system matrix must have been factored by C8GECO or C8GEFA.
!
!    The routine can also solve the system (A*) * X = B.
!
!    A division by zero will occur if the input factor contains a
!    zero on the diagonal.  Technically this indicates singularity
!    but it is often caused by improper arguments or improper
!    setting of LDA.  It will not occur if the subroutines are
!    called correctly and if C8GECO has set 0.0 < RCOND
!    or C8GEFA has set INFO == 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) A(LDA,N), the output from C8GECO or C8GEFA.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) IPVT(N), the pivot vector from C8GECO
!    or C8GEFA.
!
!    Input/output, complex ( kind = 8 ) B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer ( kind = 4 ) JOB, specifies the task.
!    0, solve A * X = B,
!    nonzero, solve (A*) * X = B, where (A*) is the conjugate transpose.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(lda,n)
  complex ( kind = 8 ) b(n)
  complex ( kind = 8 ) c8vec_dotc
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  complex t
!
!  Solve A * X = B.
!
  if ( job ==  0 ) then

    do k = 1, n - 1

      l = ipvt(k)

      if ( l /= k ) then
        call c8_swap ( b(l), b(k) )
      end if

      t = b(k)
      call c8vec_axpy ( n-k, t, a(k+1,k), 1, b(k+1), 1 )

    end do

    do k = n, 1, -1
      b(k) = b(k) / a(k,k)
      t = -b(k)
      call c8vec_axpy ( k-1, t, a(1,k), 1, b(1), 1 )
    end do
!
!  Solve (A*) * X = B.
!
  else

    do k = 1, n
      t = c8vec_dotc ( k-1, a(1,k), 1, b(1), 1 )
      b(k) = ( b(k) - t ) / conjg ( a(k,k) )
    end do

    do k = n - 1, 1, -1

      b(k) = b(k) + c8vec_dotc ( n-k, a(k+1,k), 1, b(k+1), 1 )
      l = ipvt(k)

      if ( l /=  k ) then
        call c8_swap ( b(l), b(k) )
      end if

    end do

  end if

  return
end
subroutine c8salw ( a, m, l, lda, job )

!*****************************************************************************80
!
!! C8SALW Fourier transforms the rows of a C8 rectangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) A(M,L)
!    On input, the matrix to be transformed.
!    On output, the transformed matrix.
!
!    Input, integer ( kind = 4 ) M, L, the number of rows and columns of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) JOB.
!    +1, for direct Fourier transform.
!    -1, for inverse Fourier transform.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda

  complex ( kind = 8 ) a(lda,l)
  complex ( kind = 8 ) e
  complex ( kind = 8 ) f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) job
  integer ( kind = 4 ) m
  real ( kind = 8 ) p
  complex ( kind = 8 ) r1(l)
  complex ( kind = 8 ) r2(l)
  real ( kind = 8 ) ri
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2

  if ( l <= 1 ) then
    return
  end if

  r1(1) = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )

  do i1 = 2, l

    ri = real ( i1 - 1, kind = 8 )
!
!  Minimize error in forming multiples of 2 PI.
!
    p = ( ( 201.0D+00 / 32.0D+00 ) * ri &
      + 1.93530717958647692528D-03 * ri ) / real ( l, kind = 8 )

    v1 = cos ( p )
    v2 = sin ( p )
    if ( job == -1 ) then
      v2 = -v2
    end if

    r1(i1) = cmplx ( v1, v2, kind = 8 )

  end do

  do i = 1, m

    do i1 = 1, l
      e = r1(i1)
      f = a(i,1)
      do i2 = 2, l
        f = e * f + a(i,i2)
      end do
      r2(i1) = e * f
    end do

    a(i,1:l) = r2(1:l)

  end do

  return
end
subroutine c8_swap ( x, y )

!*****************************************************************************80
!
!! C8_SWAP swaps two C8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  complex ( kind = 8 ) x
  complex ( kind = 8 ) y
  complex ( kind = 8 ) z

  z = x
  x = y
  y = z

  return
end
subroutine c8tg_sl1 ( a1, a2, b, x, c1, c2, r1, r2, r3, r5, r6, m, &
  l, lda )

!*****************************************************************************80
!
!! C8TG_SL1 solves a C8 TG linear system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) A1(M*M,L), the first row of blocks of the
!    TG matrix.  Each block is represented by columns.
!
!    Input, complex ( kind = 8 ) A2(M*M,L-1), the first column of blocks of the
!    TG matrix, beginning with the second block.  Each block is
!    represented by columns.
!
!    Input, complex ( kind = 8 ) B(M*L), the right hand side vector.
!
!    Output, complex ( kind = 8 ) X(M*L), the solution vector, which may
!    coincide with B.
!
!    Workspace, complex ( kind = 8 ) C1(M,M,L-1), C2(M,M,L-1), R1(M,M), R2(M,M),
!    R3(M,M), R5(M,M), R6(M,M).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a1(lda,l)
  complex ( kind = 8 ) a2(lda,l-1)
  complex ( kind = 8 ) b(m,l)
  complex ( kind = 8 ) c1(m,m,l-1)
  complex ( kind = 8 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) pivot(m)
  complex ( kind = 8 ) r1(m,m)
  complex ( kind = 8 ) r2(m,m)
  complex ( kind = 8 ) r3(m,m)
  complex ( kind = 8 ) r5(m,m)
  complex ( kind = 8 ) r6(m,m)
  complex ( kind = 8 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1

  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      r3(i,j) = r1(i,j)
      i3 = i3 + 1
    end do
    x(j,1) = b(j,1)
  end do

  call c8gefa ( r3, m, m, pivot, ii )

  call c8gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system
!  with the TG matrix for N = 2 through L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    n1 = n - 1
    n2 = n - 2
    i3 = 1

    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      do j = 1, m
        do i = 1, m
          c1(i,j,n1) = r2(i,j)
        end do
      end do

      do i1 = 1, n2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            do ii = 1, m
              r5(ii,j) = r5(ii,j) + c1(i,j,i2)*a2(i3,i1)
              r6(ii,j) = r6(ii,j) + c2(i,j,i1)*a1(i3,i1+1)
              i3 = i3 + 1
            end do
          end do
        end do
      end do

    end if

    do j = 1, m
      do i = 1, m
        r2(i,j) = -r5(i,j)
      end do
      call c8gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    do j = 1, m
      do i = 1, m
        r3(i,j) = r6(i,j)
        r6(i,j) = -c1(i,j,1)
      end do
    end do

    do j = 1, m
      do i = 1, m
        do ii = 1, m
          c1(ii,j,1) = c1(ii,j,1) + r2(i,j)*r3(ii,i)
        end do
      end do
    end do

    call c8gefa ( r6, m, m, pivot, ii )

    do j = 1, m
      call c8gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        do ii = 1, m
          r1(ii,j) = r1(ii,j) + r3(i,j)*r5(ii,i)
        end do
      end do
    end do

    if ( 2 < n ) then

      do j = 1, m
        do i = 1, m
          r6(i,j) = c2(i,j,1)
        end do
      end do

      do i1 = 2, n1

        if ( i1 /= n1 ) then
          do j = 1, m
            do i = 1, m
              r5(i,j) = c2(i,j,i1)
            end do
          end do
        end if

        do j = 1, m
          do i = 1, m
            c2(i,j,i1) = r6(i,j)
          end do
          do i = 1, m
            do ii = 1, m
              c2(ii,j,i1) = c2(ii,j,i1) + r3(i,j)*c1(ii,i,i1)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, m
            do ii = 1, m
              c1(ii,j,i1) = c1(ii,j,i1) + r2(i,j)*r6(ii,i)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, m
            r6(i,j) = r5(i,j)
          end do
        end do

      end do

    end if

    do j = 1, m
      do i = 1, m
        c2(i,j,1) = r3(i,j)
      end do
    end do
!
!  Compute the solution of the system with the
!  principal minor of order M*N.
!
    do j = 1, m
      do i = 1, m
        r3(i,j) = r1(i,j)
      end do
      x(j,n) = b(j,n)
    end do

    do i1 = 1, n1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        do ii = 1, m
          x(ii,n) = x(ii,n) - x(i,i2)*a2(i3,i1)
          i3 = i3 + 1
        end do
      end do
    end do

    call c8gefa ( r3, m, m, pivot, ii )
    call c8gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n1
      do i = 1, m
        do ii = 1, m
          x(ii,i1) = x(ii,i1) + x(i,n)*c2(ii,i,i1)
        end do
      end do
    end do

  end do

  return
end
subroutine c8tg_sl ( a, x, r, m, l, lda )

!*****************************************************************************80
!
!! C8TG_SL solves a linear system involving a C8 TG matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2002
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, complex ( kind = 8 ) A(LDA,2*L-1), an M*M by 2*L-2 array, containing
!    the first row of blocks of the CTG matrix, followed by its first
!    column of blocks beginning with the second block.  Each block is
!    represented by columns.
!
!    Input/output, complex ( kind = 8 ) A(M*L).  On input, the right hand side
!    vector, and on output the solution.
!
!    Workspace, complex ( kind = 8 ) R(M*M*(2*L+3)+M).
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A, which must
!    be at least M*M.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  complex ( kind = 8 ) a(lda,2*l-1)
  integer ( kind = 4 ) mm
  integer ( kind = 4 ) mml
  integer ( kind = 4 ) mml1
  integer ( kind = 4 ) mml2
  integer ( kind = 4 ) mml3
  integer ( kind = 4 ) mml4
  integer ( kind = 4 ) mml5
  complex ( kind = 8 ) r(*)
  complex ( kind = 8 ) x(m,l)

  mm = m * m
  mml = mm*(l - 1) + 1
  mml1 = 2*mml - 1
  mml2 = mml1 + mm
  mml3 = mml2 + mm
  mml4 = mml3 + mm
  mml5 = mml4 + mm

  call c8tg_sl1 ( a, a(1,l+1), x, x, r, r(mml), r(mml1), r(mml2), &
    r(mml3), r(mml4), r(mml5), m, l, lda )

  return
end
subroutine c8to_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! C8TO_MXV multiplies a C8 Toeplitz matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 8 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, complex ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 8 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  complex ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 8 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

    do j = 1, i - 1
      b(i) = b(i) + a(n+i-j) * x(j)
    end do

    do j = i, n
      b(i) = b(i) + a(j+1-i) * x(j)
    end do

  end do

  return
end
subroutine c8to_print ( n, a, title )

!*****************************************************************************80
!
!! C8TO_PRINT prints a C8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 8 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  character ( len = * ) title

  call c8to_print_some ( n, a, 1, 1, n, n, title )

  return
end
subroutine c8to_print_some ( n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! C8TO_PRINT_SOME prints some of a C8 Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, complex ( kind = 8 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 4
  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  complex ( kind = 8 ) aij
  character ( len = 20 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
!
!  Print the columns of the matrix, in strips of INCX.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i10,10x)' ) j
    end do

    write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )

    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) INCX entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+i-j)
        end if

        if ( aij == cmplx ( 0.0D+00, 0.0D+00, kind = 8 ) ) then
          ctemp(j2) = '    0.0'
        else if ( aimag ( aij ) == 0.0D+00 ) then
          write ( ctemp(j2), '(g10.3,10x)' ) real ( aij )
        else
          write ( ctemp(j2), '(2g10.3)' ) aij
        end if

      end do

      write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
subroutine c8to_random ( n, seed, a )

!*****************************************************************************80
!
!! C8TO_RANDOM randomizes a C8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 8 ) A(2*N-1), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  integer ( kind = 4 ) seed

  call c8vec_uniform_01 ( 2 * n - 1, seed, a )

  return
end
subroutine c8to_sl ( n, a, b, x, job )

!*****************************************************************************80
!
!! C8TO_SL solves the C8 Toeplitz system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 8 ) A(2*N-1), the first row of the Toeplitz matrix,
!    followed by the first column of the Toeplitz matrix, beginning with the
!    second element.
!
!    Input, complex ( kind = 8 ) B(N) the right hand side vector.
!
!    Output, complex ( kind = 8 ) X(N), the solution vector.  X and B may share
!    the same storage.
!
!    Input, integer ( kind = 4 ) JOB,
!    0 to solve A*X=B,
!    nonzero to solve A'*X=B.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  complex ( kind = 8 ) b(n)
  complex ( kind = 8 ) c1(n-1)
  complex ( kind = 8 ) c2(n-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) job
  integer ( kind = 4 ) nsub
  complex ( kind = 8 ) r1
  complex ( kind = 8 ) r2
  complex ( kind = 8 ) r3
  complex ( kind = 8 ) r5
  complex ( kind = 8 ) r6
  complex ( kind = 8 ) x(n)

  if ( n < 1 ) then
    return
  end if
!
!  Solve the system with the principal minor of order 1.
!
  r1 = a(1)
  x(1) = b(1) / r1

  if ( n == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the Toeplitz matrix.
!
  do nsub = 2, n
!
!  Compute multiples of the first and last columns of the inverse of
!  the principal minor of order NSUB.
!
    if ( job == 0 ) then
      r5 = a(n+nsub-1)
      r6 = a(nsub)
    else
      r5 = a(nsub)
      r6 = a(n+nsub-1)
    end if

    if ( 2 < nsub ) then

      c1(nsub-1) = r2

      do i = 1, nsub-2
        if ( job == 0 ) then
          r5 = r5 + a(n+i) * c1(nsub-i)
          r6 = r6 + a(i+1) * c2(i)
        else
          r5 = r5 + a(i+1) * c1(nsub-i)
          r6 = r6 + a(n+i) * c2(i)
        end if
      end do

    end if

    r2 = -r5 / r1
    r3 = -r6 / r1
    r1 = r1 + r5 * r3

    if ( 2 < nsub ) then

      r6 = c2(1)
      c2(nsub-1) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
      do i = 2, nsub - 1
        r5 = c2(i)
        c2(i) = c1(i) * r3 + r6
        c1(i) = c1(i) + r6 * r2
        r6 = r5
      end do

    end if

    c2(1) = r3
!
!  Compute the solution of the system with the principal minor of order NSUB.
!
    r5 = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

    do i = 1, nsub - 1
      if ( job == 0 ) then
        r5 = r5 + a(n+i) * x(nsub-i)
      else
        r5 = r5 + a(i+1) * x(nsub-i)
      end if
    end do

    r6 = ( b(nsub) - r5 ) / r1

    x(1:nsub-1) = x(1:nsub-1) + c2(1:nsub-1) * r6
    x(nsub) = r6

  end do

  return
end
subroutine c8to_vxm ( n, a, x, b )

!*****************************************************************************80
!
!! C8TO_VXM multiplies a vector times a C8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, complex ( kind = 8 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, complex ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, complex ( kind = 8 ) B(N), the product A' * X.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(2*n-1)
  complex ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  complex ( kind = 8 ) x(n)

  do i = 1, n

    b(i) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

    do j = 1, i
      b(i) = b(i) + a(i+1-j) * x(j)
    end do

    do j = i+1, n
      b(i) = b(i) + a(n+j-i) * x(j)
    end do

  end do

  return
end
subroutine c8trdi ( t, ldt, n, det, job, info )

!*****************************************************************************80
!
!! C8TRDI computes the determinant and inverse of a C8 triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) T(LDT,N).
!    On input, T contains the triangular matrix.  The zero elements of the
!    matrix are not referenced, and the corresponding elements of the array
!    can be used to store other information.
!    On output, T contains the inverse of the matrix, if requested.
!
!    Input, integer ( kind = 4 ) LDT, the leading dimension of T.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix T.
!
!    Output, complex ( kind = 8 ) DET(2), the determinant if requested.
!    The determinant = DET(1) * 10.0**DET(2)
!    with 1.0 <= c8_abs1(DET(1)) < 10.0 or DET(1) == 0.0.
!
!    Input, integer ( kind = 4 ) JOB, indicates the shape of the matrix,
!    and the task.
!    010, inverse of lower triangular matrix only.
!    011, inverse of upper triangular matrix only.
!    100, determinant only.
!    110, determinant and inverse of lower triangular matrix.
!    111, determinant and inverse of upper triangular matrix.
!
!    Output, integer ( kind = 4 ) INFO, inverse information.
!    If the inverse was requested, then INFO is:
!    0, if the system is nonsingular,
!    nonzero, if the system is singular.
!
  implicit none

  integer ( kind = 4 ) ldt
  integer ( kind = 4 ) n

  real ( kind = 8 ) c8_abs1
  complex ( kind = 8 ) det(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  complex ( kind = 8 ) t(ldt,n)
  complex ( kind = 8 ) temp
  real ( kind = 8 ), parameter :: ten = 10.0D+00
!
!  Determinant
!
  if ( job / 100 /= 0 ) then

    det(1) = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
    det(2) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

    do i = 1, n

      det(1) = t(i,i) * det(1)

      if ( c8_abs1 ( det(1) ) == 0.0D+00 ) then
        exit
      end if

      do while ( c8_abs1 ( det(1) ) < 1.0D+00 )
        det(1) = cmplx ( ten, 0.0D+00, kind = 8 ) * det(1)
        det(2) = det(2) - cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
      end do

      do while ( ten <= c8_abs1 ( det(1) ) )
        det(1) = det(1) / cmplx ( ten, 0.0D+00, kind = 8 )
        det(2) = det(2) + cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
      end do

    end do

  end if

  if ( mod ( job / 10, 10 ) == 0 ) then
    return
  end if
!
!  Inverse of upper triangular matrix.
!
  if ( mod ( job, 10 ) /= 0 ) then

    info = 0

    do k = 1, n

      if ( c8_abs1 ( t(k,k) ) == 0.0D+00 ) then
        info = k
        exit
      end if

      t(k,k) = cmplx ( 1.0D+00, 0.0D+00, kind = 8 ) / t(k,k)
      temp = -t(k,k)
      call c8vec_scal ( k - 1, temp, t(1,k), 1 )

      do j = k + 1, n
        temp = t(k,j)
        t(k,j) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
        call c8vec_axpy ( k, temp, t(1,k), 1, t(1,j), 1 )
      end do

    end do

  else
!
!  Inverse of lower triangular matrix.
!
    info = 0

    do k = n, 1, -1

      if ( c8_abs1 ( t(k,k) ) == 0.0D+00 ) then
        info = k
        exit
      end if

      t(k,k) = cmplx ( 1.0D+00, 0.0D+00, kind = 8 ) / t(k,k)
      temp = -t(k,k)

      if ( k /= n ) then
        call c8vec_scal ( n-k, temp, t(k+1,k), 1 )
      end if

      do j = 1, k - 1
        temp = t(k,j)
        t(k,j) = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
        call c8vec_axpy ( n-k+1, temp, t(k,k), 1, t(k,j), 1 )
      end do

    end do

  end if

  return
end
function c8vec_amax_index ( n, cx, incx )

!*****************************************************************************80
!
!! C8VEC_AMAX_INDEX indexes the C8VEC element of maximum absolute value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) C(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
!    Output, integer ( kind = 4 ) C8VEC_AMAX_INDEX, the index of the element
!    of CX of maximum absolute value.
!
  implicit none

  real ( kind = 8 ) c8_abs1
  integer ( kind = 4 ) c8vec_amax_index
  complex ( kind = 8 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) smax

  if ( n < 1 ) then
    c8vec_amax_index = 0
    return
  end if

  c8vec_amax_index = 1

  if ( n == 1 ) then
    return
  end if

  if ( incx == 1 ) then

    smax = c8_abs1 ( cx(1) )

    do i = 2, n
      if ( smax < c8_abs1 ( cx(i) ) ) then
        c8vec_amax_index = i
        smax = c8_abs1 ( cx(i) )
      end if
    end do

  else

    ix = 1
    smax = c8_abs1 ( cx(1) )
    ix = ix + incx
    do i = 2, n
      if ( smax < c8_abs1 ( cx(ix) ) ) then
        c8vec_amax_index = i
        smax = c8_abs1 ( cx(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
subroutine c8vec_axpy ( n, ca, cx, incx, cy, incy )

!*****************************************************************************80
!
!! C8VEC_AXPY adds a constant times one C8VEC to another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) CA, the multiplier.
!
!    Input, complex ( kind = 8 ) CX(*), the vector to be scaled and added to Y.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input/output, complex ( kind = 8 ) Y(*), the vector to which a multiple of
!    X is to be added.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries of Y.
!
  implicit none

  complex ( kind = 8 ) ca
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  if ( n <= 0 ) then

  else if ( ca == cmplx ( 0.0D+00, 0.0D+00, kind = 8 ) ) then

  else if ( incx == 1 .and. incy == 1 ) then

    cy(1:n) = cy(1:n) + ca * cx(1:n)

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      cy(iy) = cy(iy) + ca * cx(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
function c8vec_dotc ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! C8VEC_DOTC forms the dot product of two C8VEC's, conjugating the first.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, complex ( kind = 8 ) CX(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
!    Input, complex ( kind = 8 ) CY(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of CY.
!
!    Output, real ( kind = 8 ) C8VEC_DOTC, the (conjugated) dot product
!    of CX and CY.
!
  implicit none

  complex ( kind = 8 ) c8vec_dotc
  complex ( kind = 8 ) ctemp
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  ctemp = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  c8vec_dotc = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    ctemp = dot_product ( conjg ( cx(1:n) ), cy(1:n) )

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp = ctemp + conjg ( cx(ix) ) * cy(iy)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  c8vec_dotc = ctemp

  return
end
subroutine c8vec_indicator ( n, a )

!*****************************************************************************80
!
!! C8VEC_INDICATOR sets a C8VEC to the indicator vector.
!
!  Discussion:
!
!    X(1:N) = ( 1-1i, 2-2i, 3-3i, 4-4i, ... )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, complex ( kind = 8 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = cmplx ( i, -i, kind = 8 )
  end do

  return
end
function c8vec_nrm2 ( n, x, incx )

!*****************************************************************************80
!
!! C8VEC_NRM2 computes the unitary norm of a C8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) X(*), the vector whose norm is to be computed.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 8 ) C8VEC_NRM2, the unitary norm of X.
!
  implicit none

  real ( kind = 8 ) c8vec_nrm2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) stemp
  complex ( kind = 8 ) x(*)

  if ( n <= 0 ) then
    c8vec_nrm2 = 0.0D+00
    return
  end if

  if ( 0 <= incx ) then
    ix = 1
  else
    ix = ( - n + 1 ) * incx + 1
  end if

  stemp = 0.0D+00

  do i = 1, n
    stemp = stemp + conjg ( x(ix) ) * x(ix)
    ix = ix + incx
  end do

  c8vec_nrm2 = sqrt ( stemp )

  return
end
subroutine c8vec_print ( n, a, title )

!*****************************************************************************80
!
!! C8VEC_PRINT prints a C8VEC, with an optional title.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, complex ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(i8,2g14.6)' ) i, a(i)
  end do

  return
end
subroutine c8vec_print_some ( n, x, max_print )

!*****************************************************************************80
!
!! C8VEC_PRINT_SOME prints some of a C8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, complex ( kind = 8 ) X(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  complex ( kind = 8 ) x(n)

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,2g14.6)' ) i, x(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    i = max_print
    write ( *, '(i8,2x,2g14.6,2x,a)' ) i, x(i), '...more entries...'

  end if

  return
end
subroutine c8vec_scal ( n, ca, cx, incx )

!*****************************************************************************80
!
!! C8VEC_SCAL scales a C8VEC by a constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) CA, the multiplier.
!
!    Input/output, complex ( kind = 8 ) CX(*), the vector to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
  implicit none

  complex ( kind = 8 ) ca
  complex ( kind = 8 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    cx(1:n) = ca * cx(1:n)

  else

    do i = 1, n * incx, incx
      cx(i) = ca * cx(i)
    end do

  end if

  return
end
subroutine c8vec_scal_r8 ( n, sa, cx, incx )

!*****************************************************************************80
!
!! C8VEC_SCAL_R8 scales a C8VEC by an R8 constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input/output, complex ( kind = 8 ) CX(N), the vector to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
  implicit none

  complex ( kind = 8 ) cx(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  real ( kind = 8 ) sa

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    do i = 1, n
      cx(i) = cmplx ( sa * real ( cx(i) ), sa * aimag ( cx(i) ), kind = 8 )
    end do

  else

    nincx = n * incx
    do i = 1, nincx, incx
      cx(i) = cmplx ( sa * real ( cx(i) ), sa * aimag ( cx(i) ), kind = 8 )
    end do

  end if

  return
end
subroutine c8vec_uniform_01 ( n, seed, c )

!*****************************************************************************80
!
!! C8VEC_UNIFORM_01 returns a unit pseudorandom C8VEC.
!
!  Discussion:
!
!    A C8VEC is a vector of C8's.
!
!    The angles should be uniformly distributed between 0 and 2 * PI,
!    the square roots of the radius uniformly distributed between 0 and 1.
!
!    This results in a uniform distribution of values in the unit circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values to compute.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, complex ( kind = 8 ) C(N), the pseudorandom complex vector.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) c(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) theta

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r = sqrt ( real ( seed, kind = 8 ) * 4.656612875D-10 )

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    theta = 2.0D+00 * r8_pi * ( real ( seed, kind = 8 ) * 4.656612875D-10 )

    c(i) = r * cmplx ( cos ( theta ), sin ( theta ), kind = 8 )

  end do

  return
end
subroutine r4bto_mxv ( m, l, a1, a2, x, b )

!*****************************************************************************80
!
!! R4BTO_MXV computes the R4 block Toeplitz matrix product A * X = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!    X = (/ 1, 2, 3, 4, 5, 6 /)
!
!    B = (/ 91, 134, 73, 125, 97, 129 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 4 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 4 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, real ( kind = 4 ) X(M*L), the vector to be multiplied.
!
!    Output, real ( kind = 4 ) B(M*L), the product vector, A * X.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 4 ) a1(m,m,l)
  real ( kind = 4 ) a2(m,m,l-1)
  real ( kind = 4 ) b(m,l)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) x(m,l)
!
!  Construct the right hand side by blocks.
!
  do i = 1, l

    b(1:m,i) = 0.0E+00

    do j = 1, i - 1
      b(1:m,i) = b(1:m,i) + matmul ( a2(1:m,1:m,i-j), x(1:m,j) )
    end do

    do j = i, l
      b(1:m,i) = b(1:m,i) + matmul ( a1(1:m,1:m,j+1-i), x(1:m,j) )
    end do

  end do

  return
end
subroutine r4bto_print ( m, l, a1, a2, title )

!*****************************************************************************80
!
!! R4BTO_PRINT prints an R4 block Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 4 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 4 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 4 ) a1(m,m,l)
  real ( kind = 4 ) a2(m,m,l-1)
  character ( len = * ) title

  call r4bto_print_some ( m, l, a1, a2, 1, 1, m*l, m*l, title )

  return
end
subroutine r4bto_print_some ( m, l, a1, a2, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R4BTO_PRINT_SOME prints some of an R4 block Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 4 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 4 ) A2(M,M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the matrix, beginning with the second block.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 4 ) a1(m,m,l)
  real ( kind = 4 ) a2(m,m,l-1)
  real ( kind = 4 ) aij
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3hi
  integer ( kind = 4 ) i3lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  integer ( kind = 4 ) j3hi
  integer ( kind = 4 ) j3lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) n
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  n = m * l
!
!  Print the columns of the matrix, in strips of 5.
!
  do j3lo = jlo, jhi, incx

    j3hi = j3lo + incx - 1
    j3hi = min ( j3hi, n )
    j3hi = min ( j3hi, jhi )

    inc = j3hi + 1 - j3lo

    write ( *, '(a)' ) ' '

    do j = j3lo, j3hi
      j3 = j + 1 - j3lo
      write ( ctemp(j3), '(i7,7x)' ) j
    end do

    write ( *, '(''Columns:'',5a14)' ) ( ctemp(j3), j3 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i3lo = max ( ilo, 1 )
    i3hi = min ( ihi, n )

    do i = i3lo, i3hi
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
      do j3 = 1, inc

        j = j3lo - 1 + j3
!
!  i = M * ( i1 - 1 ) + i2
!  j = M * ( j1 - 1 ) + j2
!
        i1 = ( i - 1 ) / m + 1
        i2 = i - m * ( i1 - 1 )
        j1 = ( j - 1 ) / m + 1
        j2 = j - m * ( j1 - 1 )

        if ( i1 <= j1 ) then
          aij = a1(i2,j2,j1+1-i1)
        else
          aij = a2(i2,j2,i1-j1)
        end if

        write ( ctemp(j3), '(g14.6)' ) aij

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j3), j3 = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r4bto_sl ( m, l, a1, a2, b, x )

!*****************************************************************************80
!
!! R4BTO_SL solves the R4 block Toeplitz linear system A * X = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 4 ) A1(M*M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  Each block is represented by columns.
!
!    Input, real ( kind = 4 ) A2(M*M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the matrix, beginning with the second block.
!    Each block is represented by columns.
!
!    Input, real ( kind = 4 ) B(M*L), the right hand side vector.
!
!    Output, real ( kind = 4 ) X(M*L), the solution vector.  X and B may
!    share storage.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 4 ) a1(m*m,l)
  real ( kind = 4 ) a2(m*m,l-1)
  real ( kind = 4 ) b(m,l)
  real ( kind = 4 ) c1(m,m,l-1)
  real ( kind = 4 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) pivot(m)
  real ( kind = 4 ) r1(m,m)
  real ( kind = 4 ) r2(m,m)
  real ( kind = 4 ) r3(m,m)
  real ( kind = 4 ) r5(m,m)
  real ( kind = 4 ) r6(m,m)
  real ( kind = 4 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1
  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      i3 = i3 + 1
    end do
  end do

  r3(1:m,1:m) = r1(1:m,1:m)
  x(1:m,1) = b(1:m,1)

  call r4gefa ( r3, m, m, pivot, info )

  call r4gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system
!  with the block Toeplitz matrix for N = 2 through L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    i3 = 1
    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n-1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      c1(1:m,1:m,n-1) = r2(1:m,1:m)

      do i1 = 1, n - 2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            call r4vec_axpy ( m, c1(i,j,i2), a2(i3,i1), 1, r5(1,j), 1 )
            call r4vec_axpy ( m, c2(i,j,i1), a1(i3,i1+1), 1, r6(1,j), 1 )
            i3 = i3 + m
          end do
        end do
      end do

    end if

    do j = 1, m
      r2(1:m,j) = -r5(1:m,j)
      call r4gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    r3(1:m,1:m) = r6(1:m,1:m)
    r6(1:m,1:m) = -c1(1:m,1:m,1)

    do j = 1, m
      do i = 1, m
        call r4vec_axpy ( m, r2(i,j), r3(1,i), 1, c1(1,j,1), 1 )
      end do
    end do

    call r4gefa ( r6, m, m, pivot, info )

    do j = 1, m
      call r4gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        call r4vec_axpy ( m, r3(i,j), r5(1,i), 1, r1(1,j), 1 )
      end do
    end do

    if ( 2 < n ) then

      r6(1:m,1:m) = c2(1:m,1:m,1)

      do i1 = 2, n - 1

        if ( i1 /= n - 1 ) then
          r5(1:m,1:m) = c2(1:m,1:m,i1)
        end if

        do j = 1, m
          c2(1:m,j,i1) = r6(1:m,j)
          do i = 1, m
            call r4vec_axpy ( m, r3(i,j), c1(1,i,i1), 1, c2(1,j,i1), 1 )
          end do
        end do

        do j = 1, m
          do i = 1, m
            call r4vec_axpy ( m, r2(i,j), r6(1,i), 1, c1(1,j,i1), 1 )
          end do
        end do

        r6(1:m,1:m) = r5(1:m,1:m)

      end do

    end if

    c2(1:m,1:m,1) = r3(1:m,1:m)
!
!  Compute the solution of the system with the principal minor of order M*N.
!
    r3(1:m,1:m) = r1(1:m,1:m)
    x(1:m,n) = b(1:m,n)

    do i1 = 1, n - 1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        call r4vec_axpy ( m, -x(i,i2), a2(i3,i1), 1, x(1,n), 1 )
        i3 = i3 + m
      end do
    end do

    call r4gefa ( r3, m, m, pivot, info )

    call r4gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n - 1
      do i = 1, m
        call r4vec_axpy ( m, x(i,n), c2(1,i,i1), 1, x(1,i1), 1 )
      end do
    end do

  end do

  return
end
subroutine r4bto_to_r4ge ( m, l, a1, a2, lda, n, a )

!*****************************************************************************80
!
!! R4BTO_TO_R4GE converts an R4 block Toeplitz matrix to Linpack General form.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the R4BTO matrix.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column of
!    the R4BTO matrix.
!
!    Input, real ( kind = 4 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the R4BTO matrix.  There are L blocks, and each is
!    of order M*M.
!
!    Input, real ( kind = 4 ) A2(M,M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the R4BTO matrix, beginning with the second
!    block.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the GE matrix.
!
!    Output, integer ( kind = 4 ) N, the order of the GE matrix.
!
!    Output, real ( kind = 4 ) A(LDA,N), the N by N GE matrix.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  real ( kind = 4 ) a(lda,m*l)
  real ( kind = 4 ) a1(m,m,l)
  real ( kind = 4 ) a2(m,m,l-1)
  logical, parameter :: debug = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) n

  n = m * l

  do i = 1, n

    i1 = ( i - 1 ) / m + 1
    i2 = i - m * ( i1 - 1 )

    if ( debug ) then
      write ( *, '(a,3i8)' ) 'I:', i, i1, i2
    end if

    do j = 1, n

      j1 = ( j - 1 ) / m + 1
      j2 = j - m * ( j1 - 1 )

      if ( debug ) then
        write ( *, '(a,3i8)' ) 'J:', j, j1, j2
      end if

      if ( i1 <= j1 ) then
        a(i,j) = a1(i2,j2,j1+1-i1)
      else
        a(i,j) = a2(i2,j2,i1-j1)
      end if

    end do

  end do

  return
end
subroutine r4bto_vxm ( m, l, a1, a2, x, b )

!*****************************************************************************80
!
!! R4BTO_VXM computes the R4 block Toeplitz matrix product X * A = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!    X = (/ 1, 2, 3, 4, 5, 6 /)
!
!    B = (/ ? /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 4 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 4 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, real ( kind = 4 ) X(M*L), the vector to be multiplied.
!
!    Output, real ( kind = 4 ) B(M*L), the product vector, X * A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 4 ) a1(m,m,l)
  real ( kind = 4 ) a2(m,m,l-1)
  real ( kind = 4 ) b(m,l)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) x(m,l)
!
!  Construct the right hand side by blocks.
!
  do i = 1, l

    b(1:m,i) = 0.0E+00

    do j = 1, i
      b(1:m,i) = b(1:m,i) + matmul ( transpose ( a1(1:m,1:m,i+1-j) ), x(1:m,j) )
    end do

    do j = i+1, l
      b(1:m,i) = b(1:m,i) + matmul ( transpose ( a2(1:m,1:m,j-i) ), x(1:m,j) )
    end do

  end do

  return
end
subroutine r4cc_qr ( a, q, s, m, l, ldq, lds )

!*****************************************************************************80
!
!! R4CC_QR computes QR factorization of an R4 M by L column circulant matrix.
!
!  Discussion:
!
!    The factorization has the form A * inverse(R) = Q.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A(M), the first column of the column-circulant
!    matrix.
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrices A and Q.
!    M must be at least as large as L.
!
!    Input, integer ( kind = 4 ) L, the number of columns of the matrices A
!    and Q and the order of the upper triangular matrix S.
!
!    Input, integer ( kind = 4 ) LDQ, the leading dimension of Q.
!
!    Input, integer ( kind = 4 ) LDS, the leading dimension of S.
!
!    Output, real ( kind = 4 ) Q(LDQ,L), the M by L matrix Q of
!    the factorization.  The columns of Q are orthonormal.
!
!    Output, real ( kind = 4 ) S(LDS,L), the L by L inverse of the R matrix of
!    the factorization.  Elements below the main diagonal are not accessed.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) ldq
  integer ( kind = 4 ) lds
  integer ( kind = 4 ) m

  real ( kind = 4 ) a(m)
  real ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) ji
  real ( kind = 4 ) q(ldq,l)
  real ( kind = 4 ) r4vec_dot
  real ( kind = 4 ) r4vec_nrm2
  real ( kind = 4 ) s(lds,l)
  real ( kind = 4 ) scale
!
!  Initialization.
!  The last column of Q is used as a work vector.
!
  q(1:m,1) = a(1:m)
  q(1:m,l) = a(1:m)
!
!  Recurrent process for the lattice algorithm with normalization.
!
  do j1 = 1, l

    j = j1 + 1
    scale = 1.0E+00 / r4vec_nrm2 ( m, q(1,j1), 1 )

    if ( j1 /= l ) then

      c = - scale * ( q(m,j1) * q(1,l) + &
        r4vec_dot ( m-1, q(1,j1), 1, q(2,l), 1 ) ) / r4vec_nrm2 ( m, q(1,l), 1 )

      q(1,j) = q(m,j1) + c * q(1,l)
      do i = 2, m
        q(i,j) = q(i-1,j1) + c * q(i,l)
      end do

      if ( j /= l ) then
        q(1,l) = q(1,l) + c * q(m,j1)
        call r4vec_axpy ( m-1, c, q(1,j1), 1, q(2,l), 1 )
      end if

      s(1,j) = c

      if ( 2 < j ) then
        do i = 2, j1
          ji = j - i
          s(i,j) = s(i-1,j1) + c * s(ji,j1)
        end do
      end if

    end if

    call r4vec_scal ( m, scale, q(1,j1), 1 )
    s(j1,j1) = 1.0E+00
    call r4vec_scal ( j1, scale, s(1,j1), 1 )

  end do

  return
end
subroutine r4gefa ( a, lda, n, ipvt, info )

!*****************************************************************************80
!
!! R4GEFA factors an R4 matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) A(LDA,N).
!    On intput, the matrix to be factored.
!    On output, an upper triangular matrix and the multipliers used to
!    obtain it.
!    The factorization can be written A=L*U, where L is a product of
!    permutation and unit lower triangular matrices, and U is upper triangular.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Output, integer ( kind = 4 ) IPVT(N), the pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity indicator.
!    0, normal value.
!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
!    but it does indicate that R4GESL or R4GEDI will divide by zero if called.
!    Use RCOND in R4GECO for a reliable indication of singularity.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r4vec_amax_index
  real ( kind = 4 ) t
!
!  Gaussian elimination with partial pivoting.
!
  info = 0

  do k = 1, n - 1
!
!  Find L = pivot index.
!
    l = r4vec_amax_index ( n-k+1, a(k,k), 1 ) + k - 1
    ipvt(k) = l
!
!  Zero pivot implies this column already triangularized.
!
    if ( a(l,k) == 0.0E+00 ) then
      info = k
      cycle
    end if
!
!  Interchange if necessary.
!
    if ( l /= k ) then
      t = a(l,k)
      a(l,k) = a(k,k)
      a(k,k) = t
    end if
!
!  Compute multipliers.
!
    t = -1.0E+00 / a(k,k)
    call r4vec_scal ( n-k, t, a(k+1,k), 1 )
!
!  Row elimination with column indexing.
!
    do j = k+1, n
      t = a(l,j)
      if ( l /= k ) then
        a(l,j) = a(k,j)
        a(k,j) = t
      end if
      call r4vec_axpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )
    end do

  end do

  ipvt(n) = n

  if ( a(n,n) == 0.0E+00 ) then
    info = n
  end if

  return
end
subroutine r4gesl ( a, lda, n, ipvt, b, job )

!*****************************************************************************80
!
!! R4GESL solves an R4 general linear system A * X = B.
!
!  Discussion:
!
!    R4GESL can solve either of the systems A * X = B or ( A' ) * X = B.
!
!    The system matrix must have been factored by R4GECO or R4GEFA.
!
!    A division by zero will occur if the input factor contains a
!    zero on the diagonal.  Technically this indicates singularity
!    but it is often caused by improper arguments or improper
!    setting of LDA.  It will not occur if the subroutines are
!    called correctly and if R4GECO has set 0.0E+00 < RCOND
!    or R4GEFA has set INFO == 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A(LDA,N), the output from R4GECO or R4GEFA.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) IPVT(N), the pivot vector from R4GECO
!    or R4GEFA.
!
!    Input/output, real ( kind = 4 ) B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer ( kind = 4 ) JOB.
!    0, solve A * X = B;
!    nonzero, solve transpose ( A ) * X = B.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(lda,n)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 4 ) r4vec_dot
  real ( kind = 4 ) t
!
!  Solve A * X = B.
!
  if ( job == 0 ) then

    do k = 1, n - 1

      l = ipvt(k)
      t = b(l)

      if ( l /= k ) then
        b(l) = b(k)
        b(k) = t
      end if

      call r4vec_axpy ( n-k, t, a(k+1,k), 1, b(k+1), 1 )

    end do

    do k = n, 1, -1
      b(k) = b(k) / a(k,k)
      t = -b(k)
      call r4vec_axpy ( k-1, t, a(1,k), 1, b(1), 1 )
    end do

  else
!
!  Solve transpose ( A ) * X = B.
!
    do k = 1, n
      t = r4vec_dot ( k-1, a(1,k), 1, b(1), 1 )
      b(k) = ( b(k) - t ) / a(k,k)
    end do

    do k = n - 1, 1, -1

      b(k) = b(k) + r4vec_dot ( n-k, a(k+1,k), 1, b(k+1), 1 )
      l = ipvt(k)

      if ( l /= k ) then
        t = b(l)
        b(l) = b(k)
        b(k) = t
      end if

    end do

  end if

  return
end
subroutine r4_random ( rlo, rhi, r )

!*****************************************************************************80
!
!! R4_RANDOM returns a random R4 in a given range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) RLO, RHI, the minimum and maximum values.
!
!    Output, real ( kind = 4 ) R, the randomly chosen value.
!
  implicit none

  logical, save :: seed = .false.
  real ( kind = 4 ) r
  real ( kind = 4 ) rhi
  real ( kind = 4 ) rlo
  real ( kind = 4 ) t

  if ( .not. seed ) then
    call random_seed ( )
    seed = .true.
  end if
!
!  Pick a random number in (0,1).
!
  call random_number ( harvest = t )
!
!  Set R.
!
  r = ( 1.0E+00 - t ) * rlo + t * rhi

  return
end
subroutine r4to_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! R4TO_MXV multiplies an R4 Toeplitz matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 4 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, real ( kind = 4 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 4 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 4 ) x(n)

  do i = 1, n
    b(i) = dot_product ( a(n+i-1:n+1:-1), x(1:i-1) ) &
         + dot_product ( a(1:n+1-i), x(i:n) )
  end do

  return
end
subroutine r4to_print ( n, a, title )

!*****************************************************************************80
!
!! R4TO_PRINT prints an R4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 4 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  call r4to_print_some ( n, a, 1, 1, n, n )

  return
end
subroutine r4to_print_some ( n, a, ilo, jlo, ihi, jhi )

!*****************************************************************************80
!
!! R4TO_PRINT_SOME prints some of an R4 Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 4 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  real ( kind = 4 ) aij
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
!
!  Print the columns of the matrix, in strips of 5.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i7,7x)' ) j
    end do

    write ( *, '(a,5a14)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )

    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+i-j)
        end if

        write ( ctemp(j2), '(g14.6)' ) aij

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r4to_random ( n, seed, a )

!*****************************************************************************80
!
!! R4TO_RANDOM randomizes an R4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 4 ) A(2*N-1), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  integer ( kind = 4 ) seed

  call r4vec_uniform_01 ( 2 * n - 1, seed, a )

  return
end
subroutine r4to_sl ( n, a, b, x, job )

!*****************************************************************************80
!
!! R4TO_SL solves the R4 Toeplitz system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 4 ) A(2*N-1), the first row of the Toeplitz matrix,
!    followed by the first column of the Toeplitz matrix beginning with the
!    second element.
!
!    Input, real ( kind = 4 ) B(N) the right hand side vector.
!
!    Output, real ( kind = 4 ) X(N), the solution vector.  X and B may share the
!    same storage.
!
!    Input, integer ( kind = 4 ) JOB,
!    0 to solve A*X=B,
!    nonzero to solve Transpose(A)*X=B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  real ( kind = 4 ) b(n)
  real ( kind = 4 ) c1(n-1)
  real ( kind = 4 ) c2(n-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) job
  integer ( kind = 4 ) nsub
  real ( kind = 4 ) r1
  real ( kind = 4 ) r2
  real ( kind = 4 ) r3
  real ( kind = 4 ) r5
  real ( kind = 4 ) r6
  real ( kind = 4 ) x(n)

  if ( n < 1 ) then
    return
  end if
!
!  Solve the system with the principal minor of order 1.
!
  r1 = a(1)
  x(1) = b(1) / r1

  if ( n == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the Toeplitz matrix.
!
  do nsub = 2, n
!
!  Compute multiples of the first and last columns of the inverse of
!  the principal minor of order NSUB.
!
    if ( job == 0 ) then
      r5 = a(n+nsub-1)
      r6 = a(nsub)
    else
      r5 = a(nsub)
      r6 = a(n+nsub-1)
    end if

    if ( 2 < nsub ) then

      c1(nsub-1) = r2

      do i = 1, nsub-2
        if ( job == 0 ) then
          r5 = r5 + a(n+i) * c1(nsub-i)
          r6 = r6 + a(i+1) * c2(i)
        else
          r5 = r5 + a(i+1) * c1(nsub-i)
          r6 = r6 + a(n+i) * c2(i)
        end if
      end do

    end if

    r2 = - r5 / r1
    r3 = - r6 / r1
    r1 = r1 + r5 * r3

    if ( 2 < nsub ) then

      r6 = c2(1)
      c2(nsub-1) = 0.0E+00

      do i = 2, nsub - 1
        r5 = c2(i)
        c2(i) = c1(i) * r3 + r6
        c1(i) = c1(i) + r6 * r2
        r6 = r5
      end do

    end if

    c2(1) = r3
!
!  Compute the solution of the system with the principal minor of order NSUB.
!
    r5 = 0.0E+00
    do i = 1, nsub - 1
      if ( job == 0 ) then
        r5 = r5 + a(n+i) * x(nsub-i)
      else
        r5 = r5 + a(i+1) * x(nsub-i)
      end if
    end do

    r6 = ( b(nsub) - r5 ) / r1

    x(1:nsub-1) = x(1:nsub-1) + c2(1:nsub-1) * r6
    x(nsub) = r6

  end do

  return
end
subroutine r4to_vxm ( n, a, x, b )

!*****************************************************************************80
!
!! R4TO_VXM multiplies a vector times an R4 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 4 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, real ( kind = 4 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 4 ) B(N), the product A' * X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(2*n-1)
  real ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 4 ) x(n)

  do i = 1, n

    b(i) = dot_product ( a(i:1:-1), x(1:i) ) + &
           dot_product ( a(n+1:2*n-i), x(i+1:n) )

  end do

  return
end
subroutine r4trdi ( t, ldt, n, det, job, info )

!*****************************************************************************80
!
!! R4TRDI computes the determinant and inverse of an R4 triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) T(LDT,N).
!    On input, T contains the triangular matrix. The zero elements of the
!    matrix are not referenced, and the corresponding elements of the array
!    can be used to store other information.
!    On output, T contains the inverse matrix, if it was requested.
!
!    Input, integer ( kind = 4 ) LDT, the leading dimension of T.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix T.
!
!    Output, real ( kind = 4 ) DET(2), the determinant of the matrix, if
!    requested.  The determinant = DET(1) * 10.0^DET(2), with
!    1.0 <= abs ( DET(1) ) < 10.0, or DET(1) == 0.
!
!    Input, integer ( kind = 4 ) JOB, specifies the shape of T, and the task.
!    010, inverse of lower triangular matrix.
!    011, inverse of upper triangular matrix.
!    100, determinant only.
!    110, determinant and inverse of lower triangular.
!    111, determinant and inverse of upper triangular.
!
!    Output, integer ( kind = 4 ) INFO.
!    If the inverse was requested, then
!    0, if the system was nonsingular;
!    nonzero, if the system was singular.
!
  implicit none

  integer ( kind = 4 ) ldt
  integer ( kind = 4 ) n

  real ( kind = 4 ) det(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  real ( kind = 4 ) t(ldt,n)
  real ( kind = 4 ) temp
  real ( kind = 4 ), parameter :: ten = 10.0E+00
!
!  Determinant.
!
  if ( job / 100 /= 0 ) then

    det(1) = 1.0E+00
    det(2) = 0.0E+00

    do i = 1, n

      det(1) = t(i,i) * det(1)

      if ( det(1) == 0.0E+00 ) then
        exit
      end if

      do while ( abs ( det(1) ) < 1.0E+00 )
        det(1) = ten * det(1)
        det(2) = det(2) - 1.0E+00
      end do

      do while ( ten <= abs ( det(1) ) )
        det(1) = det(1) / ten
        det(2) = det(2) + 1.0E+00
      end do

    end do

  end if

  if ( mod ( job / 10, 10 ) == 0 ) then
    return
  end if
!
!  Inverse of an upper triangular matrix.
!
  if ( mod ( job, 10 ) /= 0 ) then

    info = 0

    do k = 1, n

      if ( t(k,k) == 0.0E+00 ) then
        info = k
        exit
      end if

      t(k,k) = 1.0E+00 / t(k,k)
      temp = -t(k,k)
      call r4vec_scal ( k-1, temp, t(1,k), 1 )

      do j = k + 1, n
        temp = t(k,j)
        t(k,j) = 0.0E+00
        call r4vec_axpy ( k, temp, t(1,k), 1, t(1,j), 1 )
      end do

    end do
!
!  Inverse of a lower triangular matrix.
!
  else

    info = 0

    do k = n, 1, -1

      if ( t(k,k) == 0.0E+00 ) then
        info = k
        exit
      end if

      t(k,k) = 1.0E+00 / t(k,k)
      temp = -t(k,k)

      if ( k /= n ) then
        call r4vec_scal ( n-k, temp, t(k+1,k), 1 )
      end if

      do j = 1, k - 1
        temp = t(k,j)
        t(k,j) = 0.0E+00
        call r4vec_axpy ( n-k+1, temp, t(k,k), 1, t(k,j), 1 )
      end do

    end do

  end if

  return
end
function r4vec_amax ( n, x, incx )

!*****************************************************************************80
!
!! R4VEC_AMAX returns the maximum absolute value of the entries in an R4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) X(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real R4VEC_AMAX, the maximum absolute value of an element of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) r4vec_amax
  real ( kind = 4 ) x(*)

  if ( n <= 0 ) then

    r4vec_amax = 0.0E+00

  else if ( n == 1 ) then

    r4vec_amax = abs ( x(1) )

  else if ( incx == 1 ) then

    r4vec_amax = abs ( x(1) )

    do i = 2, n
      if ( r4vec_amax < abs ( x(i) ) ) then
        r4vec_amax = abs ( x(i) )
      end if
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    r4vec_amax = abs ( x(ix) )
    ix = ix + incx

    do i = 2, n
      if ( r4vec_amax < abs ( x(ix) ) ) then
        r4vec_amax = abs ( x(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
function r4vec_amax_index ( n, x, incx )

!*****************************************************************************80
!
!! R4VEC_AMAX_INDEX indexes the R4VEC element of maximum absolute value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) X(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of SX.
!
!    Output, integer ( kind = 4 ) R4VEC_AMAX_INDEX, the index of the element
!    of SX of maximum absolute value.
!
  implicit none

  real ( kind = 4 ) amax
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  integer ( kind = 4 ) r4vec_amax_index
  real ( kind = 4 ) x(*)

  if ( n <= 0 ) then

    r4vec_amax_index = 0

  else if ( n == 1 ) then

    r4vec_amax_index = 1

  else if ( incx == 1 ) then

    r4vec_amax_index = 1
    amax = abs ( x(1) )

    do i = 2, n

      if ( amax < abs ( x(i) ) ) then
        r4vec_amax_index = i
        amax = abs ( x(i) )
      end if

    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    r4vec_amax_index = 1
    amax = abs ( x(ix) )

    ix = ix + incx

    do i = 2, n
      if ( amax < abs ( x(ix) ) ) then
        r4vec_amax_index = i
        amax = abs ( x(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
subroutine r4vec_axpy ( n, sa, x, incx, y, incy )

!*****************************************************************************80
!
!! R4VEC_AXPY adds a constant times one R4VEC to another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) SA, the multiplier.
!
!    Input, real ( kind = 4 ) X(*), the vector to be scaled and added to Y.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input/output, real ( kind = 4 ) Y(*), the vector to which a multiple of X
!    is to be added.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries of Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 4 ) sa
  real ( kind = 4 ) x(*)
  real ( kind = 4 ) y(*)

  if ( n <= 0 ) then

  else if ( sa == 0.0E+00 ) then

  else if ( incx == 1 .and. incy == 1 ) then

    y(1:n) = y(1:n) + sa * x(1:n)

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      y(iy) = y(iy) + sa * x(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
function r4vec_dot ( n, x, incx, y, incy )

!*****************************************************************************80
!
!! R4VEC_DOT forms the dot product of two R4VEC's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2000
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 4 ) X(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input, real ( kind = 4 ) Y(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of Y.
!
!    Output, real ( kind = 4 ) R4VEC_DOT, the dot product of X and Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 4 ) r4vec_dot
  real ( kind = 4 ) stemp
  real ( kind = 4 ) x(*)
  real ( kind = 4 ) y(*)

  if ( n <= 0 ) then

    r4vec_dot = 0.0E+00

  else if ( incx == 1 .and. incy == 1 ) then

    r4vec_dot = dot_product ( x(1:n), y(1:n) )

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    stemp = 0.0E+00
    do i = 1, n
      stemp = stemp + x(ix) * y(iy)
      ix = ix + incx
      iy = iy + incy
    end do

    r4vec_dot = stemp

  end if

  return
end
subroutine r4vec_indicator ( n, a )

!*****************************************************************************80
!
!! R4VEC_INDICATOR sets an R4VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, real ( kind = 4 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 4 )
  end do

  return
end
function r4vec_nrm2 ( n, x, incx )

!*****************************************************************************80
!
!! R4VEC_NRM2 computes the Euclidean norm of an R4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2000
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) X(*), the vector whose norm is to be computed.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 4 ) R4VEC_NRM2, the Euclidean norm of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) r4vec_amax
  real ( kind = 4 ) r4vec_nrm2
  real ( kind = 4 ) stemp
  real ( kind = 4 ) x(*)
  real ( kind = 4 ) xmax

  if ( n <= 0 ) then

    r4vec_nrm2 = 0.0E+00

  else

    xmax = r4vec_amax ( n, x, incx )

    if ( xmax == 0.0E+00 ) then

      r4vec_nrm2 = 0.0E+00

    else

      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if

      stemp = 0.0E+00
      do i = 1, n
        stemp = stemp + ( x(ix) / xmax )**2
        ix = ix + incx
      end do

      r4vec_nrm2 = xmax * sqrt ( stemp )

    end if

  end if

  return
end
subroutine r4vec_print ( n, a, title )

!*****************************************************************************80
!
!! R4VEC_PRINT prints an R4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(i8,g14.6)' ) i, a(i)
  end do

  return
end
subroutine r4vec_print_some ( n, a, max_print, title )

!*****************************************************************************80
!
!! R4VEC_PRINT_SOME prints "some" of an R4VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,g14.6)' ) i, a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    i = max_print
    write ( *, '(i8,2x,g14.6,2x,a)' ) i, a(i), '...more entries...'

  end if

  return
end
subroutine r4vec_random ( alo, ahi, n, a )

!*****************************************************************************80
!
!! R4VEC_RANDOM returns a random R4VEC in a given range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) ALO, AHI, the range allowed for the entries.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Output, real A(N), the vector of randomly chosen values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 4 ) a(n)
  real ( kind = 4 ) ahi
  real ( kind = 4 ) alo

  call random_number ( harvest = a(1:n) )

  a(1:n) = alo + a(1:n) * ( ahi - alo )

  return
end
subroutine r4vec_scal ( n, sa, x, incx )

!*****************************************************************************80
!
!! R4VEC_SCAL scales an R4VEC by a constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) SA, the multiplier.
!
!    Input/output, real ( kind = 4 ) X(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 4 ) sa
  real ( kind = 4 ) x(*)

  if ( n <= 0 ) then

  else if ( incx == 1 ) then

    m = mod ( n, 5 )

    x(1:m) = sa * x(1:m)

    do i = m+1, n, 5
      x(i)   = sa * x(i)
      x(i+1) = sa * x(i+1)
      x(i+2) = sa * x(i+2)
      x(i+3) = sa * x(i+3)
      x(i+4) = sa * x(i+4)
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    do i = 1, n
      x(ix) = sa * x(ix)
      ix = ix + incx
    end do

  end if

  return
end
subroutine r4vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R4VEC_UNIFORM_01 returns a unit pseudorandom R4VEC.
!
!  Discussion:
!
!    An R4VEC is an array of R4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value,
!    which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real ( kind = 4 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 4 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed, kind = 4 ) * 4.656612875E-10

  end do

  return
end
subroutine r8bto_mxv ( m, l, a1, a2, x, b )

!*****************************************************************************80
!
!! R8BTO_MXV computes the R8 block Toeplitz matrix product A * X = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!    X = (/ 1, 2, 3, 4, 5, 6 /)
!
!    B = (/ 91, 134, 73, 125, 97, 129 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 8 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 8 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, real ( kind = 8 ) X(M*L), the vector to be multiplied.
!
!    Output, real ( kind = 8 ) B(M*L), the product vector, A * X.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 8 ) a1(m,m,l)
  real ( kind = 8 ) a2(m,m,l-1)
  real ( kind = 8 ) b(m,l)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m,l)
!
!  Construct the right hand side by blocks.
!
  do i = 1, l

    b(1:m,i) = 0.0D+00

    do j = 1, i - 1
      b(1:m,i) = b(1:m,i) + matmul ( a2(1:m,1:m,i-j), x(1:m,j) )
    end do

    do j = i, l
      b(1:m,i) = b(1:m,i) + matmul ( a1(1:m,1:m,j+1-i), x(1:m,j) )
    end do

  end do

  return
end
subroutine r8bto_print ( m, l, a1, a2, title )

!*****************************************************************************80
!
!! R8BTO_PRINT prints an R8 block Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 8 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 8 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 8 ) a1(m,m,l)
  real ( kind = 8 ) a2(m,m,l-1)
  character ( len = * ) title

  call r8bto_print_some ( m, l, a1, a2, 1, 1, m*l, m*l, title )

  return
end
subroutine r8bto_print_some ( m, l, a1, a2, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8BTO_PRINT_SOME prints some of an R8 block Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 8 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 8 ) A2(M,M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the matrix, beginning with the second block.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 8 ) a1(m,m,l)
  real ( kind = 8 ) a2(m,m,l-1)
  real ( kind = 8 ) aij
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3hi
  integer ( kind = 4 ) i3lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  integer ( kind = 4 ) j3hi
  integer ( kind = 4 ) j3lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) n
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  n = m * l
!
!  Print the columns of the matrix, in strips of 5.
!
  do j3lo = jlo, jhi, incx

    j3hi = j3lo + incx - 1
    j3hi = min ( j3hi, n )
    j3hi = min ( j3hi, jhi )

    inc = j3hi + 1 - j3lo

    write ( *, '(a)' ) ' '

    do j = j3lo, j3hi
      j3 = j + 1 - j3lo
      write ( ctemp(j3), '(i7,7x)' ) j
    end do

    write ( *, '(''Columns:'',5a14)' ) ( ctemp(j3), j3 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i3lo = max ( ilo, 1 )
    i3hi = min ( ihi, n )

    do i = i3lo, i3hi
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
      do j3 = 1, inc

        j = j3lo - 1 + j3
!
!  i = M * ( i1 - 1 ) + i2
!  j = M * ( j1 - 1 ) + j2
!
        i1 = ( i - 1 ) / m + 1
        i2 = i - m * ( i1 - 1 )
        j1 = ( j - 1 ) / m + 1
        j2 = j - m * ( j1 - 1 )

        if ( i1 <= j1 ) then
          aij = a1(i2,j2,j1+1-i1)
        else
          aij = a2(i2,j2,i1-j1)
        end if

        write ( ctemp(j3), '(g14.6)' ) aij

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j3), j3 = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r8bto_sl ( m, l, a1, a2, b, x )

!*****************************************************************************80
!
!! R8BTO_SL solves the R8 block Toeplitz linear system A * X = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 8 ) A1(M*M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  Each block is represented by columns.
!
!    Input, real ( kind = 8 ) A2(M*M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the matrix, beginning with the second block.
!    Each block is represented by columns.
!
!    Input, real ( kind = 8 ) B(M*L), the right hand side vector.
!
!    Output, real ( kind = 8 ) X(M*L), the solution vector.  X and B may
!    share storage.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 8 ) a1(m*m,l)
  real ( kind = 8 ) a2(m*m,l-1)
  real ( kind = 8 ) b(m,l)
  real ( kind = 8 ) c1(m,m,l-1)
  real ( kind = 8 ) c2(m,m,l-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) pivot(m)
  real ( kind = 8 ) r1(m,m)
  real ( kind = 8 ) r2(m,m)
  real ( kind = 8 ) r3(m,m)
  real ( kind = 8 ) r5(m,m)
  real ( kind = 8 ) r6(m,m)
  real ( kind = 8 ) x(m,l)
!
!  Solve the system with the principal minor of order M.
!
  i3 = 1
  do j = 1, m
    do i = 1, m
      c1(i,j,1) = a1(i3,1)
      r1(i,j) = a1(i3,1)
      i3 = i3 + 1
    end do
  end do

  r3(1:m,1:m) = r1(1:m,1:m)
  x(1:m,1) = b(1:m,1)

  call r8gefa ( r3, m, m, pivot, info )

  call r8gesl ( r3, m, m, pivot, x(1,1), 0 )

  if ( l == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system
!  with the block Toeplitz matrix for N = 2 through L.
!
  do n = 2, l
!
!  Compute multiples of the first and last block columns of
!  the inverse of the principal minor of order M*N.
!
    i3 = 1
    do j = 1, m
      do i = 1, m
        r5(i,j) = a2(i3,n-1)
        r6(i,j) = a1(i3,n)
        i3 = i3 + 1
      end do
    end do

    if ( 2 < n ) then

      c1(1:m,1:m,n-1) = r2(1:m,1:m)

      do i1 = 1, n - 2
        i2 = n - i1
        do j = 1, m
          i3 = 1
          do i = 1, m
            call r8vec_axpy ( m, c1(i,j,i2), a2(i3,i1), 1, r5(1,j), 1 )
            call r8vec_axpy ( m, c2(i,j,i1), a1(i3,i1+1), 1, r6(1,j), 1 )
            i3 = i3 + m
          end do
        end do
      end do

    end if

    do j = 1, m
      r2(1:m,j) = -r5(1:m,j)
      call r8gesl ( r3, m, m, pivot, r2(1,j), 0 )
    end do

    r3(1:m,1:m) = r6(1:m,1:m)
    r6(1:m,1:m) = -c1(1:m,1:m,1)

    do j = 1, m
      do i = 1, m
        call r8vec_axpy ( m, r2(i,j), r3(1,i), 1, c1(1,j,1), 1 )
      end do
    end do

    call r8gefa ( r6, m, m, pivot, info )

    do j = 1, m
      call r8gesl ( r6, m, m, pivot, r3(1,j), 0 )
      do i = 1, m
        call r8vec_axpy ( m, r3(i,j), r5(1,i), 1, r1(1,j), 1 )
      end do
    end do

    if ( 2 < n ) then

      r6(1:m,1:m) = c2(1:m,1:m,1)

      do i1 = 2, n - 1

        if ( i1 /= n - 1 ) then
          r5(1:m,1:m) = c2(1:m,1:m,i1)
        end if

        do j = 1, m
          c2(1:m,j,i1) = r6(1:m,j)
          do i = 1, m
            call r8vec_axpy ( m, r3(i,j), c1(1,i,i1), 1, c2(1,j,i1), 1 )
          end do
        end do

        do j = 1, m
          do i = 1, m
            call r8vec_axpy ( m, r2(i,j), r6(1,i), 1, c1(1,j,i1), 1 )
          end do
        end do

        r6(1:m,1:m) = r5(1:m,1:m)

      end do

    end if

    c2(1:m,1:m,1) = r3(1:m,1:m)
!
!  Compute the solution of the system with the principal minor of order M*N.
!
    r3(1:m,1:m) = r1(1:m,1:m)
    x(1:m,n) = b(1:m,n)

    do i1 = 1, n - 1
      i2 = n - i1
      i3 = 1
      do i = 1, m
        call r8vec_axpy ( m, -x(i,i2), a2(i3,i1), 1, x(1,n), 1 )
        i3 = i3 + m
      end do
    end do

    call r8gefa ( r3, m, m, pivot, info )

    call r8gesl ( r3, m, m, pivot, x(1,n), 0 )

    do i1 = 1, n - 1
      do i = 1, m
        call r8vec_axpy ( m, x(i,n), c2(1,i,i1), 1, x(1,i1), 1 )
      end do
    end do

  end do

  return
end
subroutine r8bto_to_r8ge ( m, l, a1, a2, lda, n, a )

!*****************************************************************************80
!
!! R8BTO_TO_R8GE converts an R8 block Toeplitz matrix to a Linpack General matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the R8BTO matrix.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column of
!    the R8BTO matrix.
!
!    Input, real ( kind = 8 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the R8BTO matrix.  There are L blocks, and each is
!    of order M*M.
!
!    Input, real ( kind = 8 ) A2(M,M,L-1), the M*M by L-1 matrix containing
!    the first column of blocks of the R8BTO matrix, beginning with the second
!    block.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the GE matrix.
!
!    Output, integer ( kind = 4 ) N, the order of the GE matrix.
!
!    Output, real ( kind = 8 ) A(LDA,N), the N by N GE matrix.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m

  real ( kind = 8 ) a(lda,m*l)
  real ( kind = 8 ) a1(m,m,l)
  real ( kind = 8 ) a2(m,m,l-1)
  logical, parameter :: debug = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) n

  n = m * l

  do i = 1, n

    i1 = ( i - 1 ) / m + 1
    i2 = i - m * ( i1 - 1 )

    if ( debug ) then
      write ( *, '(a,3i8)' ) 'I:', i, i1, i2
    end if

    do j = 1, n

      j1 = ( j - 1 ) / m + 1
      j2 = j - m * ( j1 - 1 )

      if ( debug ) then
        write ( *, '(a,3i8)' ) 'J:', j, j1, j2
      end if

      if ( i1 <= j1 ) then
        a(i,j) = a1(i2,j2,j1+1-i1)
      else
        a(i,j) = a2(i2,j2,i1-j1)
      end if

    end do

  end do

  return
end
subroutine r8bto_vxm ( m, l, a1, a2, x, b )

!*****************************************************************************80
!
!! R8BTO_VXM computes the R8 block Toeplitz matrix product X * A = B.
!
!  Discussion:
!
!    The full matrix has order M * L, and can be regarded
!    as comprising L by L blocks.  Each block is of order M.
!
!    Example:
!
!      M = 2, L = 3
!
!      1 2 | 3 4 | 5 6
!      5 5 | 6 6 | 7 7
!      ----+-----+-----
!      7 8 | 1 2 | 3 4
!      8 8 | 5 5 | 6 6
!      ----+-----+-----
!      9 0 | 7 8 | 1 2
!      9 9 | 8 8 | 5 5
!
!    X = (/ 1, 2, 3, 4, 5, 6 /)
!
!    B = (/ ? /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the order of the blocks of the matrix A.
!
!    Input, integer ( kind = 4 ) L, the number of blocks in a row or column
!    of A.
!
!    Input, real ( kind = 8 ) A1(M,M,L), the M*M by L matrix containing the
!    first row of blocks of the matrix.  There are L blocks, and each is of
!    order M*M.
!
!    Input, real ( kind = 8 ) A2(M,M,L-1), the M*M by L-1 matrix containing the
!    first column of blocks of the matrix, beginning with the second block.
!
!    Input, real ( kind = 8 ) X(M*L), the vector to be multiplied.
!
!    Output, real ( kind = 8 ) B(M*L), the product vector, X * A.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m

  real ( kind = 8 ) a1(m,m,l)
  real ( kind = 8 ) a2(m,m,l-1)
  real ( kind = 8 ) b(m,l)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m,l)
!
!  Construct the right hand side by blocks.
!
  do i = 1, l

    b(1:m,i) = 0.0D+00

    do j = 1, i
      b(1:m,i) = b(1:m,i) + matmul ( transpose ( a1(1:m,1:m,i+1-j) ), x(1:m,j) )
    end do

    do j = i+1, l
      b(1:m,i) = b(1:m,i) + matmul ( transpose ( a2(1:m,1:m,j-i) ), x(1:m,j) )
    end do

  end do

  return
end
subroutine r8cc_qr ( a, q, s, m, l, ldq, lds )

!*****************************************************************************80
!
!! R8CC_QR computes QR factorization of an R8 M by L column circulant matrix.
!
!  Discussion:
!
!    The factorization has the form A * inverse(R) = Q.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(M), the first column of the column-circulant
!    matrix.
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrices A and Q.
!    M must be at least as large as L.
!
!    Input, integer ( kind = 4 ) L, the number of columns of the matrices A
!    and Q and the order of the upper triangular matrix S.
!
!    Input, integer ( kind = 4 ) LDQ, the leading dimension of Q.
!
!    Input, integer ( kind = 4 ) LDS, the leading dimension of S.
!
!    Output, real ( kind = 8 ) Q(LDQ,L), the M by L matrix Q of
!    the factorization.  The columns of Q are orthonormal.
!
!    Output, real ( kind = 8 ) S(LDS,L), the L by L inverse of the R matrix of
!    the factorization.  Elements below the main diagonal are not accessed.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) ldq
  integer ( kind = 4 ) lds
  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) ji
  real ( kind = 8 ) q(ldq,l)
  real ( kind = 8 ) r8vec_dot
  real ( kind = 8 ) r8vec_nrm2
  real ( kind = 8 ) s(lds,l)
  real ( kind = 8 ) scale
!
!  Initialization.
!  The last column of Q is used as a work vector.
!
  q(1:m,1) = a(1:m)
  q(1:m,l) = a(1:m)
!
!  Recurrent process for the lattice algorithm with normalization.
!
  do j1 = 1, l

    j = j1 + 1
    scale = 1.0D+00 / r8vec_nrm2 ( m, q(1,j1), 1 )

    if ( j1 /= l ) then

      c = - scale * ( q(m,j1) * q(1,l) + &
        r8vec_dot ( m-1, q(1,j1), 1, q(2,l), 1 ) ) / r8vec_nrm2 ( m, q(1,l), 1 )

      q(1,j) = q(m,j1) + c * q(1,l)
      do i = 2, m
        q(i,j) = q(i-1,j1) + c * q(i,l)
      end do

      if ( j /= l ) then
        q(1,l) = q(1,l) + c * q(m,j1)
        call r8vec_axpy ( m-1, c, q(1,j1), 1, q(2,l), 1 )
      end if

      s(1,j) = c

      if ( 2 < j ) then
        do i = 2, j1
          ji = j - i
          s(i,j) = s(i-1,j1) + c * s(ji,j1)
        end do
      end if

    end if

    call r8vec_scal ( m, scale, q(1,j1), 1 )
    s(j1,j1) = 1.0D+00
    call r8vec_scal ( j1, scale, s(1,j1), 1 )

  end do

  return
end
subroutine r8gefa ( a, lda, n, ipvt, info )

!*****************************************************************************80
!
!! R8GEFA factors an R8 matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) A(LDA,N).
!    On intput, the matrix to be factored.
!    On output, an upper triangular matrix and the multipliers used to
!    obtain it.
!    The factorization can be written A=L*U, where L is a product of
!    permutation and unit lower triangular matrices, and U is upper triangular.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Output, integer ( kind = 4 ) IPVT(N), the pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity indicator.
!    0, normal value.
!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
!    but it does indicate that R8GESL or R8GEDI will divide by zero if called.
!    Use RCOND in R8GECO for a reliable indication of singularity.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r8vec_amax_index
  real ( kind = 8 ) t
!
!  Gaussian elimination with partial pivoting.
!
  info = 0

  do k = 1, n - 1
!
!  Find L = pivot index.
!
    l = r8vec_amax_index ( n-k+1, a(k,k), 1 ) + k - 1
    ipvt(k) = l
!
!  Zero pivot implies this column already triangularized.
!
    if ( a(l,k) == 0.0D+00 ) then
      info = k
      cycle
    end if
!
!  Interchange if necessary.
!
    if ( l /= k ) then
      t = a(l,k)
      a(l,k) = a(k,k)
      a(k,k) = t
    end if
!
!  Compute multipliers.
!
    t = -1.0D+00 / a(k,k)
    call r8vec_scal ( n-k, t, a(k+1,k), 1 )
!
!  Row elimination with column indexing.
!
    do j = k+1, n
      t = a(l,j)
      if ( l /= k ) then
        a(l,j) = a(k,j)
        a(k,j) = t
      end if
      call r8vec_axpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )
    end do

  end do

  ipvt(n) = n

  if ( a(n,n) == 0.0D+00 ) then
    info = n
  end if

  return
end
subroutine r8gesl ( a, lda, n, ipvt, b, job )

!*****************************************************************************80
!
!! R8GESL solves an R8 general linear system A * X = B.
!
!  Discussion:
!
!    R8GESL can solve either of the systems A * X = B or ( A' ) * X = B.
!
!    The system matrix must have been factored by R8GECO or R8GEFA.
!
!    A division by zero will occur if the input factor contains a
!    zero on the diagonal.  Technically this indicates singularity
!    but it is often caused by improper arguments or improper
!    setting of LDA.  It will not occur if the subroutines are
!    called correctly and if R8GECO has set 0.0D+00 < RCOND
!    or R8GEFA has set INFO == 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(LDA,N), the output from R8GECO or R8GEFA.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) IPVT(N), the pivot vector from R8GECO
!    or R8GEFA.
!
!    Input/output, real ( kind = 8 ) B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer ( kind = 4 ) JOB.
!    0, solve A * X = B;
!    nonzero, solve transpose ( A ) * X = B.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) r8vec_dot
  real ( kind = 8 ) t
!
!  Solve A * X = B.
!
  if ( job == 0 ) then

    do k = 1, n - 1

      l = ipvt(k)
      t = b(l)

      if ( l /= k ) then
        b(l) = b(k)
        b(k) = t
      end if

      call r8vec_axpy ( n-k, t, a(k+1,k), 1, b(k+1), 1 )

    end do

    do k = n, 1, -1
      b(k) = b(k) / a(k,k)
      t = -b(k)
      call r8vec_axpy ( k-1, t, a(1,k), 1, b(1), 1 )
    end do

  else
!
!  Solve transpose ( A ) * X = B.
!
    do k = 1, n
      t = r8vec_dot ( k-1, a(1,k), 1, b(1), 1 )
      b(k) = ( b(k) - t ) / a(k,k)
    end do

    do k = n - 1, 1, -1

      b(k) = b(k) + r8vec_dot ( n-k, a(k+1,k), 1, b(k+1), 1 )
      l = ipvt(k)

      if ( l /= k ) then
        t = b(l)
        b(l) = b(k)
        b(k) = t
      end if

    end do

  end if

  return
end
subroutine r8_random ( rlo, rhi, r )

!*****************************************************************************80
!
!! R8_RANDOM returns a random R8 in a given range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RLO, RHI, the minimum and maximum values.
!
!    Output, real ( kind = 8 ) R, the randomly chosen value.
!
  implicit none

  logical, save :: seed = .false.
  real ( kind = 8 ) r
  real ( kind = 8 ) rhi
  real ( kind = 8 ) rlo
  real ( kind = 8 ) t

  if ( .not. seed ) then
    call random_seed ( )
    seed = .true.
  end if
!
!  Pick a random number in (0,1).
!
  call random_number ( harvest = t )
!
!  Set R.
!
  r = ( 1.0D+00 - t ) * rlo + t * rhi

  return
end
subroutine r8to_mxv ( n, a, x, b )

!*****************************************************************************80
!
!! R8TO_MXV multiplies an R8 Toeplitz matrix times a vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, real ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 8 ) B(N), the product A * x.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    b(i) = dot_product ( a(n+i-1:n+1:-1), x(1:i-1) ) &
         + dot_product ( a(1:n+1-i), x(i:n) )
  end do

  return
end
subroutine r8to_print ( n, a, title )

!*****************************************************************************80
!
!! R8TO_PRINT prints an R8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  call r8to_print_some ( n, a, 1, 1, n, n )

  return
end
subroutine r8to_print_some ( n, a, ilo, jlo, ihi, jhi )

!*****************************************************************************80
!
!! R8TO_PRINT_SOME prints some of an R8 Toeplitz matrix.
!
!  Discussion:
!
!    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(2*N-1), the N by N Toeplitz matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, designate the first row and
!    column, and the last row and column to be printed.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  real ( kind = 8 ) aij
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
!
!  Print the columns of the matrix, in strips of 5.
!
  do j2lo = jlo, jhi, incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i7,7x)' ) j
    end do

    write ( *, '(a,5a14)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )

    i2hi = min ( ihi, n )

    do i = i2lo, i2hi
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( i <= j ) then
          aij = a(j+1-i)
        else
          aij = a(n+i-j)
        end if

        write ( ctemp(j2), '(g14.6)' ) aij

      end do

      write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  write ( *, '(a)' ) ' '

  return
end
subroutine r8to_random ( n, seed, a )

!*****************************************************************************80
!
!! R8TO_RANDOM randomizes an R8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) A(2*N-1), the randomized matrix, with entries
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  integer ( kind = 4 ) seed

  call r8vec_uniform_01 ( 2 * n - 1, seed, a )

  return
end
subroutine r8to_sl ( n, a, b, x, job )

!*****************************************************************************80
!
!! R8TO_SL solves the R8 Toeplitz system A * X = B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Oleg Arushanian, MK Samarin,
!    Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle,
!    Wayne Cowell, Kenneth Dritz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
!    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
!    The TOEPLITZ Package User's Guide,
!    Argonne National Laboratory,
!    ANL-83-16, 1983.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(2*N-1), the first row of the Toeplitz matrix,
!    followed by the first column of the Toeplitz matrix beginning with the
!    second element.
!
!    Input, real ( kind = 8 ) B(N) the right hand side vector.
!
!    Output, real ( kind = 8 ) X(N), the solution vector.  X and B may share the
!    same storage.
!
!    Input, integer ( kind = 4 ) JOB,
!    0 to solve A*X=B,
!    nonzero to solve Transpose(A)*X=B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c1(n-1)
  real ( kind = 8 ) c2(n-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) job
  integer ( kind = 4 ) nsub
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  real ( kind = 8 ) r5
  real ( kind = 8 ) r6
  real ( kind = 8 ) x(n)

  if ( n < 1 ) then
    return
  end if
!
!  Solve the system with the principal minor of order 1.
!
  r1 = a(1)
  x(1) = b(1) / r1

  if ( n == 1 ) then
    return
  end if
!
!  Recurrent process for solving the system with the Toeplitz matrix.
!
  do nsub = 2, n
!
!  Compute multiples of the first and last columns of the inverse of
!  the principal minor of order NSUB.
!
    if ( job == 0 ) then
      r5 = a(n+nsub-1)
      r6 = a(nsub)
    else
      r5 = a(nsub)
      r6 = a(n+nsub-1)
    end if

    if ( 2 < nsub ) then

      c1(nsub-1) = r2

      do i = 1, nsub-2
        if ( job == 0 ) then
          r5 = r5 + a(n+i) * c1(nsub-i)
          r6 = r6 + a(i+1) * c2(i)
        else
          r5 = r5 + a(i+1) * c1(nsub-i)
          r6 = r6 + a(n+i) * c2(i)
        end if
      end do

    end if

    r2 = - r5 / r1
    r3 = - r6 / r1
    r1 = r1 + r5 * r3

    if ( 2 < nsub ) then

      r6 = c2(1)
      c2(nsub-1) = 0.0D+00

      do i = 2, nsub - 1
        r5 = c2(i)
        c2(i) = c1(i) * r3 + r6
        c1(i) = c1(i) + r6 * r2
        r6 = r5
      end do

    end if

    c2(1) = r3
!
!  Compute the solution of the system with the principal minor of order NSUB.
!
    r5 = 0.0D+00
    do i = 1, nsub - 1
      if ( job == 0 ) then
        r5 = r5 + a(n+i) * x(nsub-i)
      else
        r5 = r5 + a(i+1) * x(nsub-i)
      end if
    end do

    r6 = ( b(nsub) - r5 ) / r1

    x(1:nsub-1) = x(1:nsub-1) + c2(1:nsub-1) * r6
    x(nsub) = r6

  end do

  return
end
subroutine r8to_vxm ( n, a, x, b )

!*****************************************************************************80
!
!! R8TO_VXM multiplies a vector times an R8 Toeplitz matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(2*N-1), the entries of the first row of the
!    Toeplitz matrix, followed by the entries of the first column, beginning
!    with the second row.
!
!    Input, real ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 8 ) B(N), the product A' * X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2*n-1)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n

    b(i) = dot_product ( a(i:1:-1), x(1:i) ) + &
           dot_product ( a(n+1:2*n-i), x(i+1:n) )

  end do

  return
end
subroutine r8trdi ( t, ldt, n, det, job, info )

!*****************************************************************************80
!
!! R8TRDI computes the determinant and inverse of an R8 triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2001
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra, Jim Bunch, Cleve Moler,
!    Pete Stewart.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN 0-89871-172-X.
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) T(LDT,N).
!    On input, T contains the triangular matrix. The zero elements of the
!    matrix are not referenced, and the corresponding elements of the array
!    can be used to store other information.
!    On output, T contains the inverse matrix, if it was requested.
!
!    Input, integer ( kind = 4 ) LDT, the leading dimension of T.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix T.
!
!    Output, real ( kind = 8 ) DET(2), the determinant of the matrix, if
!    requested.  The determinant = DET(1) * 10.0^DET(2), with
!    1.0 <= abs ( DET(1) ) < 10.0, or DET(1) == 0.
!
!    Input, integer ( kind = 4 ) JOB, specifies the shape of T, and the task.
!    010, inverse of lower triangular matrix.
!    011, inverse of upper triangular matrix.
!    100, determinant only.
!    110, determinant and inverse of lower triangular.
!    111, determinant and inverse of upper triangular.
!
!    Output, integer ( kind = 4 ) INFO.
!    If the inverse was requested, then
!    0, if the system was nonsingular;
!    nonzero, if the system was singular.
!
  implicit none

  integer ( kind = 4 ) ldt
  integer ( kind = 4 ) n

  real ( kind = 8 ) det(2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  real ( kind = 8 ) t(ldt,n)
  real ( kind = 8 ) temp
  real ( kind = 8 ), parameter :: ten = 10.0D+00
!
!  Determinant.
!
  if ( job / 100 /= 0 ) then

    det(1) = 1.0D+00
    det(2) = 0.0D+00

    do i = 1, n

      det(1) = t(i,i) * det(1)

      if ( det(1) == 0.0D+00 ) then
        exit
      end if

      do while ( abs ( det(1) ) < 1.0D+00 )
        det(1) = ten * det(1)
        det(2) = det(2) - 1.0D+00
      end do

      do while ( ten <= abs ( det(1) ) )
        det(1) = det(1) / ten
        det(2) = det(2) + 1.0D+00
      end do

    end do

  end if

  if ( mod ( job / 10, 10 ) == 0 ) then
    return
  end if
!
!  Inverse of an upper triangular matrix.
!
  if ( mod ( job, 10 ) /= 0 ) then

    info = 0

    do k = 1, n

      if ( t(k,k) == 0.0D+00 ) then
        info = k
        exit
      end if

      t(k,k) = 1.0D+00 / t(k,k)
      temp = -t(k,k)
      call r8vec_scal ( k-1, temp, t(1,k), 1 )

      do j = k + 1, n
        temp = t(k,j)
        t(k,j) = 0.0D+00
        call r8vec_axpy ( k, temp, t(1,k), 1, t(1,j), 1 )
      end do

    end do
!
!  Inverse of a lower triangular matrix.
!
  else

    info = 0

    do k = n, 1, -1

      if ( t(k,k) == 0.0D+00 ) then
        info = k
        exit
      end if

      t(k,k) = 1.0D+00 / t(k,k)
      temp = -t(k,k)

      if ( k /= n ) then
        call r8vec_scal ( n-k, temp, t(k+1,k), 1 )
      end if

      do j = 1, k - 1
        temp = t(k,j)
        t(k,j) = 0.0D+00
        call r8vec_axpy ( n-k+1, temp, t(k,k), 1, t(k,j), 1 )
      end do

    end do

  end if

  return
end
function r8vec_amax ( n, x, incx )

!*****************************************************************************80
!
!! R8VEC_AMAX returns the maximum absolute value of the entries in an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real R8VEC_AMAX, the maximum absolute value of an element of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8vec_amax
  real ( kind = 8 ) x(*)

  if ( n <= 0 ) then

    r8vec_amax = 0.0D+00

  else if ( n == 1 ) then

    r8vec_amax = abs ( x(1) )

  else if ( incx == 1 ) then

    r8vec_amax = abs ( x(1) )

    do i = 2, n
      if ( r8vec_amax < abs ( x(i) ) ) then
        r8vec_amax = abs ( x(i) )
      end if
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    r8vec_amax = abs ( x(ix) )
    ix = ix + incx

    do i = 2, n
      if ( r8vec_amax < abs ( x(ix) ) ) then
        r8vec_amax = abs ( x(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
function r8vec_amax_index ( n, x, incx )

!*****************************************************************************80
!
!! R8VEC_AMAX_INDEX indexes the R8VEC element of maximum absolute value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of SX.
!
!    Output, integer ( kind = 4 ) R8VEC_AMAX_INDEX, the index of the element
!    of SX of maximum absolute value.
!
  implicit none

  real ( kind = 8 ) amax
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  integer ( kind = 4 ) r8vec_amax_index
  real ( kind = 8 ) x(*)

  if ( n <= 0 ) then

    r8vec_amax_index = 0

  else if ( n == 1 ) then

    r8vec_amax_index = 1

  else if ( incx == 1 ) then

    r8vec_amax_index = 1
    amax = abs ( x(1) )

    do i = 2, n

      if ( amax < abs ( x(i) ) ) then
        r8vec_amax_index = i
        amax = abs ( x(i) )
      end if

    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    r8vec_amax_index = 1
    amax = abs ( x(ix) )

    ix = ix + incx

    do i = 2, n
      if ( amax < abs ( x(ix) ) ) then
        r8vec_amax_index = i
        amax = abs ( x(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
subroutine r8vec_axpy ( n, sa, x, incx, y, incy )

!*****************************************************************************80
!
!! R8VEC_AXPY adds a constant times one R8VEC to another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input, real ( kind = 8 ) X(*), the vector to be scaled and added to Y.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input/output, real ( kind = 8 ) Y(*), the vector to which a multiple of X
!    is to be added.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries of Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 8 ) sa
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)

  if ( n <= 0 ) then

  else if ( sa == 0.0D+00 ) then

  else if ( incx == 1 .and. incy == 1 ) then

    y(1:n) = y(1:n) + sa * x(1:n)

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      y(iy) = y(iy) + sa * x(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
function r8vec_dot ( n, x, incx, y, incy )

!*****************************************************************************80
!
!! R8VEC_DOT forms the dot product of two R8VEC's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2000
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 8 ) X(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input, real ( kind = 8 ) Y(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of Y.
!
!    Output, real ( kind = 8 ) R8VEC_DOT, the dot product of X and Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8vec_dot
  real ( kind = 8 ) stemp
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)

  if ( n <= 0 ) then

    r8vec_dot = 0.0D+00

  else if ( incx == 1 .and. incy == 1 ) then

    r8vec_dot = dot_product ( x(1:n), y(1:n) )

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    stemp = 0.0D+00
    do i = 1, n
      stemp = stemp + x(ix) * y(iy)
      ix = ix + incx
      iy = iy + incy
    end do

    r8vec_dot = stemp

  end if

  return
end
subroutine r8vec_indicator ( n, a )

!*****************************************************************************80
!
!! R8VEC_INDICATOR sets an R8VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, real ( kind = 8 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 8 )
  end do

  return
end
function r8vec_nrm2 ( n, x, incx )

!*****************************************************************************80
!
!! R8VEC_NRM2 computes the Euclidean norm of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2000
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(*), the vector whose norm is to be computed.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 8 ) R8VEC_NRM2, the Euclidean norm of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8vec_amax
  real ( kind = 8 ) r8vec_nrm2
  real ( kind = 8 ) stemp
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xmax

  if ( n <= 0 ) then

    r8vec_nrm2 = 0.0D+00

  else

    xmax = r8vec_amax ( n, x, incx )

    if ( xmax == 0.0D+00 ) then

      r8vec_nrm2 = 0.0D+00

    else

      if ( 0 <= incx ) then
        ix = 1
      else
        ix = ( - n + 1 ) * incx + 1
      end if

      stemp = 0.0D+00
      do i = 1, n
        stemp = stemp + ( x(ix) / xmax )**2
        ix = ix + incx
      end do

      r8vec_nrm2 = xmax * sqrt ( stemp )

    end if

  end if

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 December 1999
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
    write ( *, '(i8,g14.6)' ) i, a(i)
  end do

  return
end
subroutine r8vec_print_some ( n, a, max_print, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME prints "some" of an R8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,g14.6)' ) i, a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    i = max_print
    write ( *, '(i8,2x,g14.6,2x,a)' ) i, a(i), '...more entries...'

  end if

  return
end
subroutine r8vec_random ( alo, ahi, n, a )

!*****************************************************************************80
!
!! R8VEC_RANDOM returns a random R8VEC in a given range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALO, AHI, the range allowed for the entries.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Output, real A(N), the vector of randomly chosen values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo

  call random_number ( harvest = a(1:n) )

  a(1:n) = alo + a(1:n) * ( ahi - alo )

  return
end
subroutine r8vec_scal ( n, sa, x, incx )

!*****************************************************************************80
!
!! R8VEC_SCAL scales an R8VEC by a constant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 1999
!
!  Author:
!
!    Original FORTRAN77 version by Charles Lawson, Richard Hanson,
!    David Kincaid, Fred Krogh.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input/output, real ( kind = 8 ) X(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) sa
  real ( kind = 8 ) x(*)

  if ( n <= 0 ) then

  else if ( incx == 1 ) then

    m = mod ( n, 5 )

    x(1:m) = sa * x(1:m)

    do i = m+1, n, 5
      x(i)   = sa * x(i)
      x(i+1) = sa * x(i+1)
      x(i+2) = sa * x(i+2)
      x(i+3) = sa * x(i+3)
      x(i+4) = sa * x(i+4)
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    do i = 1, n
      x(ix) = sa * x(ix)
      ix = ix + incx
    end do

  end if

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is an array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value,
!    which should NOT be 0.
!    On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

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
!    This code is distributed under the GNU LGPL license.
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
