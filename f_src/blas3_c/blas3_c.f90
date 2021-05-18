subroutine cgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, &
  ldc )

!*****************************************************************************80
!
!! CGEMM performs C:=alpha*A*B+beta*C, A, B, C rectangular.
!
!  CGEMM performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ),
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters:
!
!  TRANSA - character.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
!
!           Unchanged on exit.
!
!  TRANSB - character.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = conjg( B' ).
!
!           Unchanged on exit.
!
!  M      - integer.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - complex         .
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!    Input/output, complex C( LDC, n ).
!    Before input, the leading  m by n  part of the array  C must
!    contain the matrix C,  except when  beta  is zero, in which
!    case C need not be set on entry.
!    On output, the array  C  is overwritten by the  m by n  matrix
!    ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb
  integer ldc

  integer k
  integer m
  integer n
  character transa
  character transb
      complex            alpha, beta
      complex            a( lda, * ), b( ldb, * ), c( ldc, * )
      logical            lsame
      logical            conja, conjb, nota, notb
      integer            i, info, j, l, ncola, nrowa, nrowb
      complex            temp
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!  conjugated or transposed, set  CONJA and CONJB  as true if  A  and
!  B  respectively are to be  transposed but  not conjugated  and set
!  NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A
!  and the number of rows of  B  respectively.
!
      nota  = lsame ( transa, 'N' )
      notb  = lsame ( transb, 'N' )
      conja = lsame ( transa, 'C' )
      conjb = lsame ( transb, 'C' )

      if ( nota ) then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if

      if ( notb ) then
         nrowb = k
      else
         nrowb = n
      end if
!
!  Test the input.
!
      info = 0
      if (      ( .not.nota                 ) .and.  &
               ( .not.conja                ) .and.  &
               ( .not.lsame ( transa, 'T' ) )      ) then
         info = 1
      else if ( ( .not.notb                 ) .and.  &
               ( .not.conjb                ) .and.  &
               ( .not.lsame ( transb, 'T' ) )      ) then
         info = 2
      else if ( m < 0 ) then
         info = 3
      else if ( n < 0 ) then
         info = 4
      else if ( k < 0 ) then
         info = 5
      else if ( lda < max ( 1, nrowa ) ) then
         info = 8
      else if ( ldb < max ( 1, nrowb ) ) then
         info = 10
      else if ( ldc < max ( 1, m     ) ) then
         info = 13
      end if

      if ( info /= 0 ) then
         call xerbla ( 'cgemm ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( m == 0 ).or.( n == 0 ) .or.  &
          ( ( ( alpha == zero ).or.( k == 0 ) ) .and. ( beta == one ) ) ) then
        return
      end if
!
!  And when ALPHA == zero.
!
      if ( alpha == zero ) then

        if ( beta == zero ) then
          c(1:m,1:n) = zero
        else
          c(1:m,1:n) = beta * c(1:m,1:n)
        end if

        return

      end if
!
!  Start the operations.
!
      if ( notb ) then
         if ( nota ) then
!
!  Form  C := alpha*A * b + beta*C.
!
            do j = 1, n

               if ( beta == zero ) then
                 c(1:m,j) = zero
               else if ( beta/= one ) then
                 c(1:m,j) = beta * c(1:m,j)
               end if

               do l = 1, k
                 if ( b(l,j) /= zero ) then
                   temp = alpha * b(l,j)
                   c(1:m,j) = c(1:m,j) + temp * a(1:m,l)
                 end if
               end do

            end do

         else if ( conja ) then
!
!  Form  C := alpha*conjg( A' ) * b + beta*C.
!
            do j = 1, n
               do i = 1, m
                  temp = zero
                  do l = 1, k
                     temp = temp + conjg ( a(l,i) ) * b(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         else
!
!  Form  C := alpha*A' * b + beta*C
!
            do j = 1, n
               do i = 1, m

                  temp = dot_product ( a(1:k,i), b(1:k,j) )

                  if ( beta == zero ) then
                    c(i,j) = alpha * temp
                  else
                    c(i,j) = alpha * temp + beta * c(i,j)
                  end if

               end do
            end do

         end if

      else if ( nota ) then

         if ( conjb ) then
!
!  Form  C := alpha*A*conjg( B' ) + beta*C.
!
            do j = 1, n

               if ( beta == zero ) then
                 c(1:m,j) = zero
               else if ( beta/= one ) then
                 c(1:m,j) = beta * c(1:m,j)
               end if

               do l = 1, k
                  if ( b(j,l) /= zero ) then
                    temp = alpha * conjg ( b(j,l) )
                    c(1:m,j) = c(1:m,j) + temp * a(1:m,l)
                  end if
               end do

            end do
         else
!
!  Form  C := alpha*A * b' + beta*C
!
            do j = 1, n

               if ( beta == zero ) then
                 c(1:m,j) = zero
               else if ( beta /= one ) then
                 c(1:m,j) = beta * c(1:m,j)
               end if

               do l = 1, k
                 if ( b(j,l) /= zero ) then
                   temp = alpha * b(j,l)
                   c(1:m,j) = c(1:m,j) + temp * a(1:m,l)
                 end if
               end do

            end do

         end if

      else if ( conja ) then

         if ( conjb ) then
!
!  Form C := alpha*conjg( A' )*conjg( B' ) + beta*C.
!
            do j = 1, n
               do i = 1, m
                  temp = zero
                  do l = 1, k
                     temp = temp + conjg ( a(l,i) ) * conjg ( b(j,l) )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         else
!
!  Form C := alpha*conjg( A' ) * b' + beta*C
!
            do j = 1, n
               do i = 1, m
                  temp = zero
                  do l = 1, k
                     temp = temp + conjg ( a(l,i) ) * b(j,l)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         end if
      else
         if ( conjb ) then
!
!  Form  C := alpha*A'*conjg( B' ) + beta*C
!
            do j = 1, n
               do i = 1, m
                  temp = zero
                  do l = 1, k
                     temp = temp + a(l,i)*conjg ( b(j,l) )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         else
!
!  Form  C := alpha*A' * b' + beta*C
!
            do j = 1, n
               do i = 1, m
                  temp = zero
                  do l = 1, k
                     temp = temp + a(l,i) * b(j,l)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         end if
      end if

  return
end
subroutine chemm ( side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! CHEMM performs C:= alpha*A*B+beta*C, for A hermitian.
!
!  CHEMM performs one of the matrix-matrix operations
!
!     C := alpha*A * B + beta*C,
!
!  or
!
!     C := alpha * B*A + beta*C,
!
!  where alpha and beta are scalars, A is an hermitian matrix and  B and
!  C are m by n matrices.
!
!  Parameters:
!
!  SIDE   - character.
!           On entry,  SIDE  specifies whether  the  hermitian matrix A
!           appears on the  left or right  in the  operation as follows:
!
!              SIDE = 'L' or 'l'   C := alpha*A * b + beta*C,
!
!              SIDE = 'R' or 'r'   C := alpha * b*A + beta*C,
!
!           Unchanged on exit.
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of  the  hermitian  matrix A  is  to  be
!           referenced as follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of the
!                                  hermitian matrix is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of the
!                                  hermitian matrix is to be referenced.
!
!           Unchanged on exit.
!
!  M      - integer.
!           On entry,  M  specifies the number of rows of the matrix C.
!           M  must be at least zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry, N specifies the number of columns of the matrix C.
!           N  must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           m  when  SIDE = 'L' or 'l'  and is n  otherwise.
!           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
!           the array  A  must contain the  hermitian matrix,  such that
!           when  UPLO = 'U' or 'u', the leading m by m upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  hermitian matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  m by m  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  hermitian
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.
!           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
!           the array  A  must contain the  hermitian matrix,  such that
!           when  UPLO = 'U' or 'u', the leading n by n upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  hermitian matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  n by n  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  hermitian
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.
!           Note that the imaginary parts  of the diagonal elements need
!           not be set, they are assumed to be zero.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in the  calling (sub) program. When  SIDE = 'L' or 'l'  then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least max( 1, n ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, n ).
!           Before entry, the leading  m by n part of the array  B  must
!           contain the matrix B.
!           Unchanged on exit.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!  BETA   - complex         .
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n updated
!           matrix.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb
  integer ldc

  integer m
  integer n
  character side
  character uplo
      complex            alpha, beta
      complex            a( lda, * ), b( ldb, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, k, nrowa
      complex            temp1, temp2
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Set NROWA as the number of rows of A.
!
      if ( lsame ( side, 'L' ) ) then
         nrowa = m
      else
         nrowa = n
      end if
      upper = lsame ( uplo, 'U' )
!
!  Test the input.
!
      info = 0

      if (      ( .not.lsame ( side, 'L' ) ) .and.  &
               ( .not.lsame ( side, 'R' ) )      ) then
         info = 1
      else if ( ( .not.upper              ) .and.  &
               ( .not.lsame ( uplo, 'L' ) )      ) then
         info = 2
      else if ( m  <0               ) then
         info = 3
      else if ( n  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldb<max ( 1, m     ) ) then
         info = 9
      else if ( ldc<max ( 1, m     ) ) then
         info = 12
      end if

      if ( info /= 0 ) then
         call xerbla ( 'chemm ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( m == 0 ).or.( n == 0 ) .or.  &
          ( ( alpha == zero ) .and. ( beta == one ) ) ) then
         return
      end if
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         if ( beta == zero ) then
            c(1:m,1:n) = zero
         else
            do j = 1, n
               do i = 1, m
                  c(i,j) = beta * c(i,j)
               end do
            end do
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( side, 'L' ) ) then
!
!  Form  C := alpha*A * b + beta*C.
!
         if ( upper ) then
            do j = 1, n
               do i = 1, m
                  temp1 = alpha * b(i,j)
                  temp2 = zero
                  do k = 1, i - 1
                     c(k,j) = c(k,j) + temp1 * a( k,i)
                     temp2     = temp2     + &
                                 b(k,j)*conjg (  a(k,i) )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = temp1 * real( a(i,i) ) + &
                                 alpha * temp2
                  else
                     c(i,j) = beta * c(i,j)         + &
                                 temp1 * real( a(i,i) ) + &
                                 alpha * temp2
                  end if
               end do
            end do
         else
            do j = 1, n
               do i = m, 1, -1
                  temp1 = alpha * b(i,j)
                  temp2 = zero
                  do k = i + 1, m
                     c(k,j) = c(k,j) + temp1 * a( k,i)
                     temp2     = temp2     + &
                                 b(k,j) * conjg (  a(k,i) )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = temp1 * real( a(i,i) ) + &
                                 alpha * temp2
                  else
                     c(i,j) = beta * c(i,j)         + &
                                 temp1 * real( a(i,i) ) + &
                                 alpha * temp2
                  end if
               end do
            end do
         end if
      else
!
!  Form  C := alpha * b*A + beta*C.
!
         do j = 1, n
            temp1 = alpha * real( a(j,j) )
            if ( beta == zero ) then
               do i = 1, m
                  c(i,j) = temp1 * b(i,j)
               end do
            else
               do i = 1, m
                  c(i,j) = beta * c(i,j) + temp1 * b(i,j)
               end do
            end if
            do k = 1, j - 1
               if ( upper ) then
                  temp1 = alpha * a(k,j)
               else
                  temp1 = alpha * conjg ( a(j,k) )
               end if
               do i = 1, m
                  c(i,j) = c(i,j) + temp1 * b(i,k)
               end do
            end do
            do k = j + 1, n
               if ( upper ) then
                  temp1 = alpha * conjg ( a(j,k) )
               else
                  temp1 = alpha * a(k,j)
               end if
               do i = 1, m
                  c(i,j) = c(i,j) + temp1 * b(i,k)
               end do
            end do
         end do
      end if

  return
end
subroutine cher2k ( uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! CHER2K performs C := alpha*A*conjg(B')+conjg(alpha)*b*conjg(A')+beta*C, for C hermitian.
!
!  CHER2K performs one of the hermitian rank 2k operations
!
!     C := alpha*A*conjg( B' ) + conjg( alpha ) * b*conjg( A' ) + beta*C,
!
!  or
!
!     C := alpha*conjg( A' ) * b + conjg( alpha )*conjg( B' )*A + beta*C,
!
!  where  alpha and beta  are scalars with  beta  real,  C is an  n by n
!  hermitian matrix and  A and B  are  n by k matrices in the first case
!  and  k by n  matrices in the second case.
!
!  Parameters:
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - character.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'    C := alpha*A*conjg( B' )          +
!                                         conjg( alpha ) * b*conjg( A' ) +
!                                         beta*C.
!
!              TRANS = 'C' or 'c'    C := alpha*conjg( A' ) * b          +
!                                         conjg( alpha )*conjg( B' )*A +
!                                         beta*C.
!
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
!           matrices  A and B.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - real            .
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  hermitian matrix and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  hermitian matrix and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!           Note that the imaginary parts of the diagonal elements need
!           not be set,  they are assumed to be zero,  and on exit they
!           are set to zero.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb
  integer ldc

  character trans
  character uplo
  integer            n, k
  real               beta
  complex            alpha
      complex            a( lda, * ), b( ldb, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, l, nrowa
      complex            temp1, temp2
      real, parameter :: one  = 1.0E+00
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Test the input.
!
      if ( lsame ( trans, 'N' ) ) then
         nrowa = n
      else
         nrowa = k
      end if
      upper = lsame ( uplo, 'U' )

      info = 0
      if (      ( .not.upper               ) .and.  &
               ( .not.lsame ( uplo , 'L' ) )      ) then
         info = 1
      else if ( ( .not.lsame ( trans, 'N' ) ) .and.  &
               ( .not.lsame ( trans, 'C' ) )      ) then
         info = 2
      else if ( n  <0               ) then
         info = 3
      else if ( k  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldb<max ( 1, nrowa ) ) then
         info = 9
      else if ( ldc<max ( 1, n     ) ) then
         info = 12
      end if
      if ( info/=0 ) then
         call xerbla ( 'cher2k', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( n == 0 ).or. &
          ( ( ( alpha == zero ).or.( k == 0 ) ) .and. ( beta == one ) ) ) &
         return
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         if ( upper ) then
            if ( beta == real( zero ) ) then
               do j = 1, n
                  c(1:j,j) = zero
               end do
            else
               do j = 1, n
                  c(1:j-1,j) = beta * c(1:j-1,j)
                  c(j,j) = beta * real( c( j,j) )
               end do
            end if
         else
            if ( beta == real( zero ) ) then
               do j = 1, n
                  c(j:n,j) = zero
               end do
            else
               do j = 1, n
                  c(j,j) = beta * real( c( j,j) )
                  do i = j + 1, n
                     c(i,j) = beta * c(i,j)
                  end do
               end do
            end if
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( trans, 'N' ) ) then
!
!  Form  C := alpha*A*conjg( B' ) + conjg( alpha ) * b*conjg( A' ) + C.
!
         if ( upper ) then
            do j = 1, n
               if ( beta == real( zero ) ) then
                  do i = 1, j
                     c(i,j) = zero
                  end do
               else if ( beta/= one ) then
                  do i = 1, j - 1
                     c(i,j) = beta * c(i,j)
                  end do
                  c(j,j) = beta * real( c( j,j) )
               else
                  c(j,j) = real( c( j,j) )
               end if
               do l = 1, k
                  if ( ( a(j,l) /= zero ).or. ( b(j,l) /= zero ) ) then
                     temp1 = alpha * conjg ( b(j,l) )
                     temp2 = conjg ( alpha * a(j,l) )
                     do i = 1, j - 1
                        c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
                     end do
                     c(j,j) = real( c( j,j) )         + &
                                 real( a(j,l) * temp1 + b(j,l) * temp2   )
                  end if
               end do
            end do
         else
            do j = 1, n
               if ( beta == real( zero ) ) then
                  do i = j, n
                     c(i,j) = zero
                  end do
               else if ( beta/= one ) then
                  do i = j + 1, n
                     c(i,j) = beta * c(i,j)
                  end do
                  c(j,j) = beta * real( c( j,j) )
               else
                  c(j,j) = real( c( j,j) )
               end if
               do l = 1, k
                  if ( ( a(j,l) /= zero ).or. ( b(j,l) /= zero ) ) then
                     temp1 = alpha * conjg ( b(j,l) )
                     temp2 = conjg ( alpha * a(j,l) )
                     do i = j + 1, n
                        c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
                     end do
                     c(j,j) = real( c( j,j) )         + &
                                 real( a(j,l) * temp1 + b(j,l) * temp2   )
                  end if
               end do
            end do
         end if
      else
!
!  Form  C := alpha*conjg( A' ) * b + conjg( alpha )*conjg( B' )*A + C.
!
         if ( upper ) then
            do j = 1, n
               do i = 1, j
                  temp1 = zero
                  temp2 = zero
                  do l = 1, k
                     temp1 = temp1 + conjg ( a(l,i) ) * b(l,j)
                     temp2 = temp2 + conjg ( b(l,i) ) * a(l,j)
                  end do
                  if ( i == j ) then
                     if ( beta == real( zero ) ) then
                        c(j,j) = real(        alpha   * temp1 + &
                                          conjg ( alpha ) * temp2   )
                     else
                        c(j,j) = beta * real( c( j,j) )         + &
                                    real(        alpha   * temp1 + &
                                          conjg ( alpha ) * temp2   )
                     end if
                  else
                     if ( beta == real( zero ) ) then
                        c(i,j) = alpha * temp1 + conjg ( alpha ) * temp2
                     else
                        c(i,j) = beta *c(i,j) + &
                                    alpha * temp1 + conjg ( alpha ) * temp2
                     end if
                  end if
               end do
            end do
         else
            do j = 1, n
               do i = j, n
                  temp1 = zero
                  temp2 = zero
                  do l = 1, k
                     temp1 = temp1 + conjg ( a(l,i) ) * b(l,j)
                     temp2 = temp2 + conjg ( b(l,i) ) * a(l,j)
                  end do
                  if ( i == j ) then
                     if ( beta == real( zero ) ) then
                        c(j,j) = real(        alpha   * temp1 + &
                                          conjg ( alpha ) * temp2   )
                     else
                        c(j,j) = beta * real( c( j,j) )         + &
                                    real(        alpha   * temp1 + &
                                          conjg ( alpha ) * temp2   )
                     end if
                  else
                     if ( beta == real( zero ) ) then
                        c(i,j) = alpha * temp1 + conjg ( alpha ) * temp2
                     else
                        c(i,j) = beta *c(i,j) + &
                                    alpha * temp1 + conjg ( alpha ) * temp2
                     end if
                  end if
               end do
            end do
         end if
      end if

  return
end
subroutine cherk ( uplo, trans, n, k, alpha, a, lda, beta, c, ldc )

!*****************************************************************************80
!
!! CHERK performs C := alpha*A*conjg( A' ) + beta*C, C hermitian.
!
!  CHERK performs one of the hermitian rank k operations
!
!     C := alpha*A*conjg( A' ) + beta*C,
!
!  or
!
!     C := alpha*conjg( A' )*A + beta*C,
!
!  where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
!  matrix and  A  is an  n by k  matrix in the  first case and a  k by n
!  matrix in the second case.
!
!  Parameters:
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - character.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   C := alpha*A*conjg( A' ) + beta*C.
!
!              TRANS = 'C' or 'c'   C := alpha*conjg( A' )*A + beta*C.
!
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns   of  the   matrix A,   and  on   entry   with
!           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
!           matrix A.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real            .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - real            .
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  hermitian matrix and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  hermitian matrix and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!           Note that the imaginary parts of the diagonal elements need
!           not be set,  they are assumed to be zero,  and on exit they
!           are set to zero.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
!
  implicit none

  integer lda
  integer ldc

      character        uplo, trans
      integer            n, k
      real               alpha, beta
      complex            a( lda, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, l, nrowa
      real               rtemp
      complex            temp
      real, parameter :: one = 1.0E+00
      real, parameter :: zero = 0.0E+00
!
!  Test the input.
!
      if ( lsame ( trans, 'N' ) ) then
         nrowa = n
      else
         nrowa = k
      end if

      upper = lsame ( uplo, 'U' )

      info = 0
      if (      ( .not.upper               ) .and.  &
               ( .not.lsame ( uplo , 'L' ) )      ) then
         info = 1
      else if ( ( .not.lsame ( trans, 'N' ) ) .and.  &
               ( .not.lsame ( trans, 'C' ) )      ) then
         info = 2
      else if ( n  <0               ) then
         info = 3
      else if ( k  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldc<max ( 1, n     ) ) then
         info = 10
      end if
      if ( info/=0 ) then
         call xerbla ( 'cherk ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( n == 0 ).or. &
          ( ( ( alpha == zero ).or.( k == 0 ) ) .and. ( beta == one ) ) ) &
         return
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         if ( upper ) then
            if ( beta == zero ) then
               do j = 1, n
                  do i = 1, j
                     c(i,j) = zero
                  end do
               end do
            else
               do j = 1, n
                  do i = 1, j - 1
                     c(i,j) = beta * c(i,j)
                  end do
                  c(j,j) = beta * real( c( j,j) )
               end do
            end if
         else
            if ( beta == zero ) then
               do j = 1, n
                  do i = j, n
                     c(i,j) = zero
                  end do
               end do
            else
               do j = 1, n
                  c(j,j) = beta * real( c( j,j) )
                  do i = j + 1, n
                     c(i,j) = beta * c(i,j)
                  end do
               end do
            end if
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( trans, 'N' ) ) then
!
!  Form  C := alpha*A*conjg( A' ) + beta*C.
!
         if ( upper ) then
            do j = 1, n
               if ( beta == zero ) then
                  do i = 1, j
                     c(i,j) = zero
                  end do
               else if ( beta/= one ) then
                  do i = 1, j - 1
                     c(i,j) = beta * c(i,j)
                  end do
                  c(j,j) = beta * real( c( j,j) )
               else
                  c(j,j) = real( c( j,j) )
               end if
               do l = 1, k
                  if ( a(j,l) /= cmplx ( zero ) ) then
                     temp = alpha * conjg ( a(j,l) )
                     do i = 1, j - 1
                        c(i,j) = c(i,j) + temp * a(i,l)
                     end do
                     c(j,j) = real( c( j,j)      ) + &
                                 real( temp * a(i,l) )
                  end if
               end do
            end do
         else
            do j = 1, n
               if ( beta == zero ) then
                  c(j:n,j) = zero
               else if ( beta/= one ) then
                  c(j,j) = beta * real( c( j,j) )
                  c(j+1:n,j) = beta * c(j+1:n,j)
               else
                  c(j,j) = real( c( j,j) )
               end if
               do l = 1, k
                  if ( a(j,l) /= cmplx ( zero ) ) then
                     temp = alpha * conjg ( a(j,l) )
                     c(j,j) = real ( c( j,j)      )   + &
                                 real ( temp * a(j,l) )
                     do i = j + 1, n
                        c(i,j) = c(i,j) + temp * a(i,l)
                     end do
                  end if
               end do
            end do
         end if
      else
!
!  Form  C := alpha*conjg( A' )*A + beta*C.
!
         if ( upper ) then
            do j = 1, n
               do i = 1, j - 1
                  temp = zero
                  do l = 1, k
                     temp = temp + conjg ( a(l,i) ) * a(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
               rtemp = zero
               do l = 1, k
                  rtemp = rtemp + conjg ( a(l,j) ) * a(l,j)
               end do
               if ( beta == zero ) then
                  c(j,j) = alpha * rtemp
               else
                  c(j,j) = alpha * rtemp + beta * real( c(j,j) )
               end if
            end do
         else
            do j = 1, n
               rtemp = zero
               do l = 1, k
                  rtemp = rtemp + conjg ( a(l,j) ) * a(l,j)
               end do
               if ( beta == zero ) then
                  c(j,j) = alpha * rtemp
               else
                  c(j,j) = alpha * rtemp + beta * real( c(j,j) )
               end if
               do i = j + 1, n
                  temp = zero
                  do l = 1, k
                     temp = temp + conjg ( a(l,i) ) * a(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         end if
      end if

  return
end
subroutine csymm ( side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! CSYMM performs C := alpha*A*B+beta*C, A symmetric.
!
!  CSYMM performs one of the matrix-matrix operations
!
!     C := alpha*A * B + beta*C,
!
!  or
!
!     C := alpha * B*A + beta*C,
!
!  where  alpha and beta are scalars, A is a symmetric matrix and  B and
!  C are m by n matrices.
!
!  Parameters:
!
!  SIDE   - character.
!           On entry,  SIDE  specifies whether  the  symmetric matrix A
!           appears on the  left or right  in the  operation as follows:
!
!              SIDE = 'L' or 'l'   C := alpha*A * b + beta*C,
!
!              SIDE = 'R' or 'r'   C := alpha * b*A + beta*C,
!
!           Unchanged on exit.
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of  the  symmetric  matrix A  is  to  be
!           referenced as follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of the
!                                  symmetric matrix is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of the
!                                  symmetric matrix is to be referenced.
!
!           Unchanged on exit.
!
!  M      - integer.
!           On entry,  M  specifies the number of rows of the matrix C.
!           M  must be at least zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry, N specifies the number of columns of the matrix C.
!           N  must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           m  when  SIDE = 'L' or 'l'  and is n  otherwise.
!           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
!           the array  A  must contain the  symmetric matrix,  such that
!           when  UPLO = 'U' or 'u', the leading m by m upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  symmetric matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  m by m  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  symmetric
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.
!           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
!           the array  A  must contain the  symmetric matrix,  such that
!           when  UPLO = 'U' or 'u', the leading n by n upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  symmetric matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  n by n  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  symmetric
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in the  calling (sub) program. When  SIDE = 'L' or 'l'  then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least max( 1, n ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, n ).
!           Before entry, the leading  m by n part of the array  B  must
!           contain the matrix B.
!           Unchanged on exit.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!  BETA   - complex         .
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n updated
!           matrix.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb
  integer ldc

      character        side, uplo
      integer            m, n
      complex            alpha, beta
      complex            a( lda, * ), b( ldb, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, k, nrowa
      complex            temp1, temp2
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Set NROWA as the number of rows of A.
!
      if ( lsame ( side, 'L' ) ) then
         nrowa = m
      else
         nrowa = n
      end if

      upper = lsame ( uplo, 'U' )
!
!  Test the input.
!
      info = 0
      if (      ( .not.lsame ( side, 'L' ) ) .and.  &
               ( .not.lsame ( side, 'R' ) )      ) then
         info = 1
      else if ( ( .not.upper              ) .and.  &
               ( .not.lsame ( uplo, 'L' ) )      ) then
         info = 2
      else if ( m  <0               ) then
         info = 3
      else if ( n  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldb<max ( 1, m     ) ) then
         info = 9
      else if ( ldc<max ( 1, m     ) ) then
         info = 12
      end if
      if ( info/=0 ) then
         call xerbla ( 'csymm ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( m == 0 ).or.( n == 0 ) .or.  &
          ( ( alpha == zero ) .and. ( beta == one ) ) ) &
         return
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         if ( beta == zero ) then
            c(1:m,1:n) = zero
         else
            do j = 1, n
               do i = 1, m
                  c(i,j) = beta * c(i,j)
               end do
            end do
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( side, 'L' ) ) then
!
!  Form  C := alpha*A * b + beta*C.
!
         if ( upper ) then
            do j = 1, n
               do i = 1, m
                  temp1 = alpha * b(i,j)
                  temp2 = zero
                  do k = 1, i - 1
                     c(k,j) = c(k,j) + temp1     * a( k, i )
                     temp2     = temp2     + b(k,j) * a(k, i )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = temp1 * a(i,i) + alpha * temp2
                  else
                     c(i,j) = beta *c(i,j) + &
                                 temp1 * a(i,i) + alpha * temp2
                  end if
               end do
            end do
         else
            do j = 1, n
               do i = m, 1, -1
                  temp1 = alpha * b(i,j)
                  temp2 = zero
                  do k = i + 1, m
                     c(k,j) = c(k,j) + temp1     * a( k, i )
                     temp2     = temp2     + b(k,j) * a(k, i )
                  end do
                  if ( beta == zero ) then
                     c(i,j) = temp1 * a( i, i ) + alpha * temp2
                  else
                     c(i,j) = beta * c(i,j) + &
                                 temp1 * a( i, i ) + alpha * temp2
                  end if
               end do
            end do
         end if
      else
!
!  Form  C := alpha * b*A + beta*C.
!
         do j = 1, n

            temp1 = alpha * a(j,j)
            if ( beta == zero ) then
               do i = 1, m
                  c(i,j) = temp1 * b(i,j)
               end do
            else
               do i = 1, m
                  c(i,j) = beta * c(i,j) + temp1 * b(i,j)
               end do
            end if

            do k = 1, j - 1
               if ( upper ) then
                  temp1 = alpha * a(k,j)
               else
                  temp1 = alpha * a(j,k)
               end if
               do i = 1, m
                  c(i,j) = c(i,j) + temp1 * b(i,k)
               end do
            end do

            do k = j + 1, n
               if ( upper ) then
                  temp1 = alpha * a(j,k)
               else
                  temp1 = alpha * a(k,j)
               end if
               do i = 1, m
                  c(i,j) = c(i,j) + temp1 * b(i,k)
               end do
            end do

         end do

      end if

  return
end
subroutine csyr2k ( uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! CSYR2K performs C := alpha*A*B' + alpha*B*A' + beta*C, C symmetric.
!
!  CSYR2K performs one of the symmetric rank 2k operations
!
!     C := alpha*A * B' + alpha*B*A' + beta*C,
!
!  or
!
!     C := alpha*A' * b + alpha*B'*A + beta*C,
!
!  where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
!  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
!  matrices in the second case.
!
!  Parameters:
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - character.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'    C := alpha*A * b' + alpha*B*A' +
!                                         beta*C.
!
!              TRANS = 'T' or 't'    C := alpha*A' * b + alpha*B'*A +
!                                         beta*C.
!
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'T' or 't',  K  specifies  the number of rows of the
!           matrices  A and B.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - complex         .
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  symmetric matrix and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  symmetric matrix and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb
  integer ldc

      character        uplo, trans
      integer            n, k
      complex            alpha, beta
      complex            a( lda, * ), b( ldb, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, l, nrowa
      complex            temp1, temp2
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Test the input.
!
      if ( lsame ( trans, 'N' ) ) then
         nrowa = n
      else
         nrowa = k
      end if
      upper = lsame ( uplo, 'U' )

      info = 0
      if (      ( .not.upper               ) .and.  &
               ( .not.lsame ( uplo , 'L' ) )      ) then
         info = 1
      else if ( ( .not.lsame ( trans, 'N' ) ) .and.  &
               ( .not.lsame ( trans, 'T' ) )      ) then
         info = 2
      else if ( n  <0               ) then
         info = 3
      else if ( k  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldb<max ( 1, nrowa ) ) then
         info = 9
      else if ( ldc<max ( 1, n     ) ) then
         info = 12
      end if

      if ( info/=0 ) then
         call xerbla ( 'csyr2k', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( n == 0 ).or. &
          ( ( ( alpha == zero ).or.( k == 0 ) ) .and. ( beta == one ) ) ) &
         return
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         if ( upper ) then
            if ( beta == zero ) then
               do j = 1, n
                  c(1:j,j) = zero
               end do
            else
               do j = 1, n
                  c(1:j,j) = beta * c(1:j,j)
               end do
            end if
         else
            if ( beta == zero ) then
               do j = 1, n
                  c(j:n,j) = zero
               end do
            else
               do j = 1, n
                  c(j:n,j) = beta * c(j:n,j)
               end do
            end if
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( trans, 'N' ) ) then
!
!  Form  C := alpha*A * b' + alpha*B*A' + C.
!
         if ( upper ) then

            do j = 1, n

               if ( beta == zero ) then
                  c(1:j,j) = zero
               else if ( beta/= one ) then
                  c(1:j,j) = beta * c(1:j,j)
               end if

               do l = 1, k

                  if ( ( a(j,l) /= zero ).or. ( b(j,l) /= zero ) ) then
                     temp1 = alpha * b(j,l)
                     temp2 = alpha * a(j,l)
                     do i = 1, j
                        c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
                     end do
                  end if

               end do

            end do

         else

            do j = 1, n
               if ( beta == zero ) then
                  c(j:n,j) = zero
               else if ( beta/= one ) then
                  c(j:n,j) = beta * c(j:n,j)
               end if
               do l = 1, k
                  if ( ( a(j,l) /= zero ).or. ( b(j,l) /= zero ) ) then
                     temp1 = alpha * b(j,l)
                     temp2 = alpha * a(j,l)
                     do i = j, n
                        c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
                     end do
                  end if
               end do
            end do

         end if
      else
!
!  Form  C := alpha*A' * b + alpha*B'*A + C.
!
         if ( upper ) then

            do j = 1, n
               do i = 1, j
                  temp1 = zero
                  temp2 = zero
                  do l = 1, k
                     temp1 = temp1 + a(l,i) * b(l,j)
                     temp2 = temp2 + b(l,i) * a(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp1 + alpha * temp2
                  else
                     c(i,j) = beta *c(i,j) + &
                                 alpha * temp1 + alpha * temp2
                  end if
               end do
            end do

         else

            do j = 1, n
               do i = j, n
                  temp1 = zero
                  temp2 = zero
                  do l = 1, k
                     temp1 = temp1 + a(l,i) * b(l,j)
                     temp2 = temp2 + b(l,i) * a(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp1 + alpha * temp2
                  else
                     c(i,j) = beta * c(i,j) + alpha * temp1 + alpha * temp2
                  end if
               end do
            end do

         end if

      end if

  return
end
subroutine csyrk ( uplo, trans, n, k, alpha, a, lda, beta, c, ldc )

!*****************************************************************************80
!
!! CSYRK performs C := alpha*A*A' + beta*C, C is symmetric.
!
!  CSYRK performs one of the symmetric rank k operations
!
!     C := alpha*A*A' + beta*C,
!
!  or
!
!     C := alpha*A'*A + beta*C,
!
!  where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
!  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
!  in the second case.
!
!  Parameters:
!
!  UPLO   - character.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - character.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
!
!              TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
!
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns   of  the   matrix A,   and  on   entry   with
!           TRANS = 'T' or 't',  K  specifies  the number of rows of the
!           matrix A.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - complex         .
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - complex          array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  symmetric matrix and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  symmetric matrix and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldc

      character        uplo, trans
      integer            n, k
      complex            alpha, beta
      complex            a( lda, * ), c( ldc, * )
      logical            lsame
      logical            upper
      integer            i, info, j, l, nrowa
      complex            temp
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Test the input.
!
      if ( lsame ( trans, 'N' ) ) then
         nrowa = n
      else
         nrowa = k
      end if
      upper = lsame ( uplo, 'U' )

      info = 0
      if (      ( .not.upper               ) .and.  &
               ( .not.lsame ( uplo , 'L' ) )      ) then
         info = 1
      else if ( ( .not.lsame ( trans, 'N' ) ) .and.  &
               ( .not.lsame ( trans, 'T' ) )      ) then
         info = 2
      else if ( n  <0               ) then
         info = 3
      else if ( k  <0               ) then
         info = 4
      else if ( lda<max ( 1, nrowa ) ) then
         info = 7
      else if ( ldc<max ( 1, n     ) ) then
         info = 10
      end if
      if ( info/=0 ) then
         call xerbla ( 'csyrk ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( n == 0 ).or. &
          ( ( ( alpha == zero ).or.( k == 0 ) ) .and. ( beta == one ) ) ) &
         return
!
!  And when ALPHA == zero.
!
      if ( alpha == zero ) then
         if ( upper ) then
            if ( beta == zero ) then
               do j = 1, n
                  c(1:j,j) = zero
               end do
            else
               do j = 1, n
                  c(1:j,j) = beta * c(1:j,j)
               end do
            end if
         else
            if ( beta == zero ) then
               do j = 1, n
                  c(j:n,j) = zero
               end do
            else
               do j = 1, n
                  do i = j, n
                     c(i,j) = beta * c(i,j)
                  end do
               end do
            end if
         end if
         return
      end if
!
!  Start the operations.
!
      if ( lsame ( trans, 'N' ) ) then
!
!  Form C := alpha*A*A' + beta*C.
!
         if ( upper ) then
            do j = 1, n
               if ( beta == zero ) then
                  c(1:j,j) = zero
               else if ( beta/= one ) then
                  c(1:j,j) = beta * c(1:j,j)
               end if
               do l = 1, k
                  if ( a(j,l) /= zero ) then
                     temp = alpha * a(j,l)
                     do i = 1, j
                        c(i,j) = c(i,j) + temp * a(i,l)
                     end do
                  end if
               end do
            end do
         else
            do j = 1, n
               if ( beta == zero ) then
                  c(j:n,j) = zero
               else if ( beta/= one ) then
                  do i = j, n
                     c(i,j) = beta * c(i,j)
                  end do
               end if
               do l = 1, k
                  if ( a(j,l) /= zero ) then
                     temp = alpha * a(j,l)
                     do i = j, n
                        c(i,j) = c(i,j) + temp * a(i,l)
                     end do
                  end if
               end do
            end do
         end if
      else
!
!  Form  C := alpha*A'*A + beta*C.
!
         if ( upper ) then
            do j = 1, n
               do i = 1, j
                  temp = zero
                  do l = 1, k
                     temp = temp + a(l,i) * a(l,j)
                  end do
                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if
               end do
            end do
         else
            do j = 1, n
               do i = j, n
                  temp = zero
                  do l = 1, k
                     temp = temp + a(l,i) * a(l,j)
                  end do

                  if ( beta == zero ) then
                     c(i,j) = alpha * temp
                  else
                     c(i,j) = alpha * temp + beta * c(i,j)
                  end if

               end do
            end do
         end if
      end if

  return
end
subroutine ctrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

!*****************************************************************************80
!
!! CTRMM performs B := alpha*op( A ) * B. where A is triangular.
!
!  CTRMM performs one of the matrix-matrix operations
!
!     B := alpha*op( A ) * B,   or   B := alpha*B*op( A )
!
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
!
!  Parameters:
!
!  SIDE   - character.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:
!
!              SIDE = 'L' or 'l'   B := alpha*op( A ) * b.
!
!              SIDE = 'R' or 'r'   B := alpha * b*op( A ).
!
!           Unchanged on exit.
!
!  UPLO   - character.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - character.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
!
!           Unchanged on exit.
!
!  DIAG   - character.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - integer.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix B,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb

  character        side, uplo, transa, diag
      integer            m, n
      complex            alpha
      complex            a( lda, * ), b( ldb, * )
      logical            lsame
      logical            lside, noconj, nounit, upper
      integer            i, info, j, k, nrowa
      complex            temp
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Test the input.
!
      lside  = lsame ( side  , 'L' )

      if ( lside ) then
         nrowa = m
      else
         nrowa = n
      end if

      noconj = lsame ( transa, 'T' )
      nounit = lsame ( diag  , 'N' )
      upper  = lsame ( uplo  , 'U' )

      info   = 0
      if (      ( .not.lside                ) .and.  &
               ( .not.lsame ( side  , 'R' ) )      ) then
         info = 1
      else if ( ( .not.upper                ) .and.  &
               ( .not.lsame ( uplo  , 'L' ) )      ) then
         info = 2
      else if ( ( .not.lsame ( transa, 'N' ) ) .and.  &
               ( .not.lsame ( transa, 'T' ) ) .and.  &
               ( .not.lsame ( transa, 'C' ) )      ) then
         info = 3
      else if ( ( .not.lsame ( diag  , 'U' ) ) .and.  &
               ( .not.lsame ( diag  , 'N' ) )      ) then
         info = 4
      else if ( m  <0               ) then
         info = 5
      else if ( n  <0               ) then
         info = 6
      else if ( lda<max ( 1, nrowa ) ) then
         info = 9
      else if ( ldb<max ( 1, m     ) ) then
         info = 11
      end if

      if ( info /= 0 ) then
         call xerbla ( 'ctrmm ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( n == 0 ) then
         return
      end if
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         b(1:m,1:n) = zero
         return
      end if
!
!  Start the operations.
!
      if ( lside ) then

         if ( lsame ( transa, 'N' ) ) then
!
!  Form  B := alpha*A * b.
!
            if ( upper ) then
               do j = 1, n
                  do k = 1, m
                     if ( b(k,j) /= zero ) then
                        temp = alpha * b(k,j)
                        do i = 1, k - 1
                           b(i,j) = b(i,j) + temp * a(i,k)
                        end do
                        if ( nounit ) then
                           temp = temp * a(k,k)
                        end if
                        b(k,j) = temp
                     end if
                  end do
               end do
            else
               do j = 1, n
                  do k = m, 1, -1
                     if ( b(k,j) /= zero ) then
                        temp = alpha * b(k,j)
                        b(k,j) = temp
                        if ( nounit ) then
                           b(k,j) = b(k,j) * a(k,k)
                        end if
                        do i = k + 1, m
                           b(i,j) = b(i,j) + temp * a(i,k)
                        end do
                     end if
                  end do
               end do
            end if
         else
!
!  Form  B := alpha*A' * b   or   B := alpha*conjg( A' )*B.
!
            if ( upper ) then
               do j = 1, n
                  do i = m, 1, -1
                     temp = b(i,j)
                     if ( noconj ) then
                        if ( nounit ) then
                           temp = temp * a(i,i)
                        end if
                        do k = 1, i - 1
                           temp = temp + a(k,i) * b(k,j)
                        end do
                     else
                        if ( nounit ) then
                           temp = temp*conjg ( a(i,i) )
                        end if
                        do k = 1, i - 1
                           temp = temp + conjg ( a(k,i) ) * b(k,j)
                        end do
                     end if
                     b(i,j) = alpha * temp
                  end do
               end do
            else
               do j = 1, n
                  do i = 1, m
                     temp = b(i,j)
                     if ( noconj ) then
                        if ( nounit ) then
                           temp = temp * a(i,i)
                        end if
                        do k = i + 1, m
                           temp = temp + a(k,i) * b(k,j)
                        end do
                     else
                        if ( nounit ) then
                           temp = temp * conjg ( a(i,i) )
                        end if
                        do k = i + 1, m
                           temp = temp + conjg ( a(k,i) ) * b(k,j)
                        end do
                     end if
                     b(i,j) = alpha * temp
                  end do
               end do
            end if
         end if
      else
         if ( lsame ( transa, 'N' ) ) then
!
!  Form B := alpha * b*A.
!
            if ( upper ) then
               do j = n, 1, -1
                  temp = alpha
                  if ( nounit ) then
                     temp = temp * a(j,j)
                  end if
                  do i = 1, m
                     b(i,j) = temp * b(i,j)
                  end do
                  do k = 1, j - 1
                     if ( a(k,j) /= zero ) then
                        temp = alpha * a(k,j)
                        do i = 1, m
                           b(i,j) = b(i,j) + temp * b(i,k)
                        end do
                     end if
                  end do
               end do
            else
               do j = 1, n
                  temp = alpha
                  if ( nounit ) then
                     temp = temp * a(j,j)
                  end if
                  do i = 1, m
                     b(i,j) = temp * b(i,j)
                  end do
                  do k = j + 1, n
                     if ( a(k,j) /= zero ) then
                        temp = alpha * a(k,j)
                        do i = 1, m
                           b(i,j) = b(i,j) + temp * b(i,k)
                        end do
                     end if
                  end do
               end do
            end if
         else
!
!  Form  B := alpha * b*A'   or   B := alpha*B*conjg( A' ).
!
            if ( upper ) then
               do k = 1, n
                  do j = 1, k - 1
                     if ( a(j,k) /= zero ) then
                        if ( noconj ) then
                           temp = alpha * a(j,k)
                        else
                           temp = alpha * conjg ( a(j,k) )
                        end if
                        do i = 1, m
                           b(i,j) = b(i,j) + temp * b(i,k)
                        end do
                     end if
                  end do
                  temp = alpha
                  if ( nounit ) then
                     if ( noconj ) then
                        temp = temp * a(k,k)
                     else
                        temp = temp*conjg ( a(k,k) )
                     end if
                  end if
                  if ( temp /= one ) then
                     do i = 1, m
                        b(i,k) = temp * b(i,k)
                     end do
                  end if
               end do
            else
               do k = n, 1, -1
                  do j = k + 1, n
                     if ( a(j,k) /= zero ) then
                        if ( noconj ) then
                           temp = alpha * a(j,k)
                        else
                           temp = alpha * conjg ( a(j,k) )
                        end if
                        do i = 1, m
                           b(i,j) = b(i,j) + temp * b(i,k)
                        end do
                     end if
                  end do
                  temp = alpha
                  if ( nounit ) then
                     if ( noconj ) then
                        temp = temp * a(k,k)
                     else
                        temp = temp * conjg ( a(k,k) )
                     end if
                  end if
                  if ( temp /= one ) then
                     do i = 1, m
                        b(i,k) = temp * b(i,k)
                     end do
                  end if
               end do
            end if
         end if
      end if

  return
end
subroutine ctrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

!*****************************************************************************80
!
!! CTRSM solves op( A ) * x = alpha * B, where A is triangular.
!
!  CTRSM solves one of the matrix equations
!
!     op( A ) * x = alpha * B,   or   X*op( A ) = alpha*B,
!
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
!
!  The matrix X is overwritten on B.
!
!  Parameters:
!
!  SIDE   - character.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:
!
!              SIDE = 'L' or 'l'   op( A ) * x = alpha * b.
!
!              SIDE = 'R' or 'r'   X*op( A ) = alpha * b.
!
!           Unchanged on exit.
!
!  UPLO   - character.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - character.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
!
!           Unchanged on exit.
!
!  DIAG   - character.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - integer.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - complex         .
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - complex          array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - integer.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - complex          array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix B,  and  on exit  is
!           overwritten by the solution matrix X.
!
!  LDB    - integer.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
  implicit none

  integer lda
  integer ldb

      character        side, uplo, transa, diag
      integer            m, n
      complex            alpha
      complex            a( lda, * ), b( ldb, * )
      logical            lsame
      logical            lside, noconj, nounit, upper
      integer            i, info, j, k, nrowa
      complex            temp
      complex, parameter :: one = ( 1.0E+00, 0.0E+00 )
      complex, parameter :: zero = ( 0.0E+00, 0.0E+00 )
!
!  Test the input.
!
      lside  = lsame ( side  , 'L' )
      if ( lside ) then
         nrowa = m
      else
         nrowa = n
      end if
      noconj = lsame ( transa, 'T' )
      nounit = lsame ( diag  , 'N' )
      upper  = lsame ( uplo  , 'U' )

      info   = 0
      if (      ( .not.lside                ) .and.  &
               ( .not.lsame ( side  , 'R' ) )      ) then
         info = 1
      else if ( ( .not.upper                ) .and.  &
               ( .not.lsame ( uplo  , 'L' ) )      ) then
         info = 2
      else if ( ( .not.lsame ( transa, 'N' ) ) .and.  &
               ( .not.lsame ( transa, 'T' ) ) .and.  &
               ( .not.lsame ( transa, 'C' ) )      ) then
         info = 3
      else if ( ( .not.lsame ( diag  , 'U' ) ) .and.  &
               ( .not.lsame ( diag  , 'N' ) )      ) then
         info = 4
      else if ( m  <0               ) then
         info = 5
      else if ( n  <0               ) then
         info = 6
      else if ( lda<max ( 1, nrowa ) ) then
         info = 9
      else if ( ldb<max ( 1, m     ) ) then
         info = 11
      end if
      if ( info/=0 ) then
         call xerbla ( 'ctrsm ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( n == 0 ) then
         return
      end if
!
!  And when  alpha == zero.
!
      if ( alpha == zero ) then
         b(1:m,1:n) = zero
         return
      end if
!
!  Start the operations.
!
      if ( lside ) then
         if ( lsame ( transa, 'N' ) ) then
!
!  Form  B := alpha*inv( A ) * b.
!
            if ( upper ) then
               do j = 1, n
                  if ( alpha /= one ) then
                     do i = 1, m
                        b(i,j) = alpha * b(i,j)
                     end do
                  end if
                  do k = m, 1, -1
                     if ( b(k,j) /= zero ) then
                        if ( nounit ) then
                           b(k,j) = b(k,j) / a(k,k)
                        end if
                        do i = 1, k - 1
                           b(i,j) = b(i,j) - b(k,j) * a(i,k)
                        end do
                     end if
                  end do
               end do
            else
               do j = 1, n
                  if ( alpha /= one ) then
                     do i = 1, m
                        b(i,j) = alpha * b(i,j)
                     end do
                  end if
                  do k = 1, m
                     if ( b(k,j) /= zero ) then
                        if ( nounit ) then
                           b(k,j) = b(k,j) / a(k,k)
                        end if
                        do i = k + 1, m
                           b(i,j) = b(i,j) - b(k,j) * a(i,k)
                        end do
                     end if
                  end do
               end do
            end if
         else
!
!  Form  B := alpha*inv( A' ) * b
!  or    B := alpha*inv( conjg( A' ) ) * b.
!
            if ( upper ) then
               do j = 1, n
                  do i = 1, m
                     temp = alpha * b(i,j)
                     if ( noconj ) then
                        do k = 1, i - 1
                           temp = temp - a(k,i) * b(k,j)
                        end do
                        if ( nounit ) then
                           temp = temp / a(i,i)
                        end if
                     else
                        do k = 1, i - 1
                           temp = temp - conjg ( a(k,i) ) * b(k,j)
                        end do
                        if ( nounit ) then
                           temp = temp / conjg ( a(i,i) )
                        end if
                     end if
                     b(i,j) = temp
                  end do
               end do
            else
               do j = 1, n
                  do i = m, 1, -1
                     temp = alpha * b(i,j)
                     if ( noconj ) then
                        do k = i + 1, m
                           temp = temp - a(k,i) * b(k,j)
                        end do
                        if ( nounit ) then
                           temp = temp/a(i,i)
                        end if
                     else
                        do k = i + 1, m
                           temp = temp - conjg ( a(k,i) ) * b(k,j)
                        end do
                        if ( nounit ) then
                           temp = temp / conjg ( a(i,i) )
                        end if
                     end if
                     b(i,j) = temp
                  end do
               end do
            end if
         end if
      else
         if ( lsame ( transa, 'N' ) ) then
!
!  Form  B := alpha * b*inv( A ).
!
            if ( upper ) then
               do j = 1, n
                  if ( alpha /= one ) then
                     do i = 1, m
                        b(i,j) = alpha * b(i,j)
                     end do
                  end if
                  do k = 1, j - 1
                     if ( a(k,j) /= zero ) then
                        do i = 1, m
                           b(i,j) = b(i,j) - a(k,j) * b(i,k)
                        end do
                     end if
                  end do
                  if ( nounit ) then
                     temp = one / a(j,j)
                     do i = 1, m
                        b(i,j) = temp * b(i,j)
                     end do
                  end if
               end do
            else
               do j = n, 1, -1
                  if ( alpha/= one ) then
                     do i = 1, m
                        b(i,j) = alpha * b(i,j)
                     end do
                  end if
                  do k = j + 1, n
                     if ( a(k,j) /= zero ) then
                        do i = 1, m
                           b(i,j) = b(i,j) - a(k,j) * b(i,k)
                        end do
                     end if
                  end do
                  if ( nounit ) then
                     temp = one/a(j,j)
                     do i = 1, m
                       b(i,j) = temp * b(i,j)
                     end do
                  end if
               end do
            end if
         else
!
!  Form  B := alpha * b*inv( A' )
!  or    B := alpha * b*inv( conjg( A' ) ).
!
            if ( upper ) then
               do k = n, 1, -1
                  if ( nounit ) then
                     if ( noconj ) then
                        temp = one/a(k,k)
                     else
                        temp = one/conjg ( a(k,k) )
                     end if
                     do i = 1, m
                        b(i,k) = temp * b(i,k)
                     end do
                  end if
                  do j = 1, k - 1
                     if ( a(j,k) /= zero ) then
                        if ( noconj ) then
                           temp = a(j,k)
                        else
                           temp = conjg ( a(j,k) )
                        end if
                        do i = 1, m
                           b(i,j) = b(i,j) - temp * b(i,k)
                        end do
                     end if
                  end do
                  if ( alpha/= one ) then
                     do i = 1, m
                        b(i,k) = alpha * b(i,k)
                     end do
                  end if
               end do
            else
               do k = 1, n
                  if ( nounit ) then
                     if ( noconj ) then
                        temp = one/a(k,k)
                     else
                        temp = one/conjg ( a(k,k) )
                     end if
                     do i = 1, m
                        b(i,k) = temp * b(i,k)
                     end do
                  end if
                  do j = k + 1, n
                     if ( a(j,k) /= zero ) then
                        if ( noconj ) then
                           temp = a(j,k)
                        else
                           temp = conjg ( a(j,k) )
                        end if
                        do i = 1, m
                           b(i,j) = b(i,j) - temp * b(i,k)
                        end do
                     end if
                  end do
                  if ( alpha/= one ) then
                     do i = 1, m
                        b(i,k) = alpha * b(i,k)
                     end do
                  end if
               end do
            end if
         end if
      end if

  return
end
