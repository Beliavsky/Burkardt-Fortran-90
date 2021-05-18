recursive function iota ( x ) result ( value )

!*****************************************************************************80
!
!! iota() is a recursive function.
!
  if ( x <= 0.0 ) then
    value = 0.0
  else
    value = x + iota ( x - 1 )
  end if

  return
end
