subroutine byname ( action, name, value_in, value_out )

!*****************************************************************************80
!
!! byname() controls a set of named persistent data.
!
!  Discussion:
!
!    Three values are stored, named ALPHA, BETA and gamma.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    string ACTION (case insensitive, only first character important)
!    'Get'     get the value
!    'Print'   print the value
!    'Reset'   reset value to default
!    'Set'     set the value
!
!    string NAME, the name of the parameter (case insensitive)
!    'ALPHA'
!    'BETA'
!    'gamma'
!    '*' (all variables)
!    NAME is required for 'Get' and 'Set'.  
!    NAME, if omitted, is assumed '*' for 'Print' and 'Reset'.
!
!    VALUE_IN.
!    Only used for 'Set' command.
!
!  Output:
!
!    VALUE_OUT.
!    If NAME was specified on input, and was not '*', then
!    VALUE_OUT is the current value of the corresponding variable.
!    Otherwise, VALUE_OUT is empty.
!
  implicit none

  character ( len = * ), optional :: action
  character ( len = 1 ) :: action2
  real ( kind = 8 ), save :: alpha = 1.0D+00
  real ( kind = 8 ), save :: alpha_default = 1.0D+00
  real ( kind = 8 ), save :: beta = 2.0D+00
  real ( kind = 8 ), save :: beta_default = 2.0D+00
  real ( kind = 8 ), save :: gamma = 3.0D+00
  real ( kind = 8 ), save :: gamma_default = 3.0D+00
  logical match
  character ( len = * ), optional :: name
  character ( len = 1 ) :: name2
  real ( kind = 8 ), optional :: value_in
  real ( kind = 8 ), optional :: value_out
!
!  No arguments: default 'print *'
!
  if ( .not. present ( action ) ) then
    action2 = 'p'
  else
    action2 = action(1:1)
    call ch_low ( action2 )
    match = &
      action2 == 'g' .or. &
      action2 == 'p' .or. &
      action2 == 'r' .or. &
      action2 == 's'
    if ( .not. match ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'byname: Fatal error!'
      write ( *, '(a)' ) '  Legal actions are "g", "p", "r", "s".'
      write ( *, '(a)' ) '  not "' // action //'".'
      stop ( 1 )
    end if
  end if

  if ( ( action2 == 'p' .or. action2 == 'r' ) .and. .not. present ( name ) ) then
    name2 = '*'
  else
    name2 = name(1:1)
    call ch_low ( name2 )
    match = &
      name2 == 'a' .or. &
      name2 == 'b' .or. &
      name2 == 'g' .or. &
      name2 == '*'

    if ( .not. match ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'byname: Fatal error!'
      write ( *, '(a)' ) '  Legal names are "alpha", "beta", "gamma", "*".'
      write ( *, '(a)' ) '  not "' // name // '"!'
      stop ( 1 )
    end if

  end if
!
!  Reject missing value.
!
  if ( action2 == 's' .and. .not. present ( value_in ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'byname: Fatal error!'
    write ( *, '(a)' ) '  "set, name" command but no "value".'
    stop ( 1 )
  end if
!
!  Carry out requested action.
!
  if ( action2 == 'g' ) then
    if ( name2 == 'a' ) then
      value_out = alpha
    else if ( name2 == 'b' ) then
      value_out = beta
    else if ( name2 == 'g' ) then
      value_out = gamma
    end if
  else if ( action2 == 'p' ) then
    if ( name2 == 'a' .or. name2 == '*' ) then
      write ( *, '(a,g14.6)' ) '  alpha = ', alpha
    end if
    if ( name2 == 'a' .and. present ( value_out ) ) then
      value_out = alpha
    end if
    if ( name2 == 'b' .or. name2 == '*' ) then
      write ( *, '(a,g14.6)' ) '  beta  = ', beta
    end if
    if ( name2 == 'beta' .and. present ( value_out ) ) then
      value_out = beta
    end if
    if ( name2 == 'g' .or. name2 == '*' ) then
      write ( *, '(a,g14.6)' ) '  gamma = ', gamma
    end if
    if ( name2 == 'g' .and. present ( value_out ) ) then
      value_out = gamma
    end if
  else if ( action2 == 'r' ) then
    if ( name2 == 'a' .or. name2 == '*' ) then
      alpha = alpha_default
    end if
    if ( name2 == 'a' .and. present ( value_out ) ) then
      value_out = alpha
    end if
    if ( name2 == 'b' .or. name2 == '*' ) then
      beta = beta_default
    end if
    if ( name2 == 'beta' .and. present ( value_out ) ) then
      value_out = beta
    end if
    if ( name2 == 'g' .or. name2 == '*' ) then
      gamma = gamma_default
    end if
    if ( name2 == 'g' .and. present ( value_out ) ) then
      value_out = gamma
    end if
  else if ( action2 == 's' ) then
    if ( name2 == 'a' ) then
      alpha = value_in
      if ( present ( value_out ) ) then
        value_out = alpha
      end if
    end if
    if ( name2 == 'b' ) then
      beta = value_in
      if ( present ( value_out ) ) then
        value_out = beta
      end if
    end if
    if ( name2 == 'g' ) then
      gamma = value_in
      if ( present ( value_out ) ) then
        value_out = gamma
      end if
    end if
  end if

  return
end
subroutine ch_low ( ch )

!*****************************************************************************80
!
!! ch_low() lowercases a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character CH, the character to be lowercased.
!
!  Output:
!
!    character CH, the lowercased character.
!
  implicit none

  character ch
  integer ( kind = 4 ) i

  i = iachar ( ch )

  if ( 65 <= i .and. i <= 90 ) then
    ch = achar ( i + 32 )
  end if

  return
end

