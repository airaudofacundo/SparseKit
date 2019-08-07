Module quickSortMod
  !Takes 1, 2, 3, 4.. arrays of same dimension, orders first one and modifies the others acordingly
  Use tools
  Implicit none
  Public
  Interface swap
     Module Procedure swapInt
     Module Procedure swapReal
  End Interface swap
  Interface quickSort
     Module Procedure quickSort1vect
     Module Procedure quickSort2vect
     Module Procedure quickSort3vect
     Module Procedure quickSort4vect
  End Interface quickSort
Contains

  Recursive Subroutine quickSort1vect(vect1, low, high)
    Implicit none
    Real(dp), Dimension(:), Intent(InOut) :: vect1
    Integer, Intent(In) :: low, high
    Integer i, iPivot, j
    iPivot = high    
    i = low
    Do While(iPivot > i)
       If (vect1(i) > vect1(iPivot)) then
          Call swap(vect1(i), vect1(iPivot-1))
          Call swap(vect1(iPivot-1), vect1(iPivot))
          iPivot = iPivot - 1
       Else 
          i=i+1
       End If
    End Do
    If (low < high) then
       Call quickSort(vect1, low, iPivot-1)
       Call quickSort(vect1, iPivot+1, high)
    End If       
  End Subroutine quickSort1vect

  Recursive Subroutine quickSort2vect(vect1, vect2, low, high)
    Implicit none
    Integer, Dimension(:), Intent(InOut) :: vect1
    Real(dp), Dimension(:), Intent(InOut) :: vect2
    Integer, Intent(In) :: low, high
    Integer i, iPivot, j
    iPivot = high    
    i = low
    Do While(iPivot > i)
       If (vect1(i) > vect1(iPivot)) then
          Call swap(vect1(i), vect1(iPivot-1))
          Call swap(vect2(i), vect2(iPivot-1))
          Call swap(vect1(iPivot-1), vect1(iPivot))
          Call swap(vect2(iPivot-1), vect2(iPivot))
          iPivot = iPivot - 1
       Else 
          i=i+1
       End If
    End Do
    If (low < high) then
       Call quickSort(vect1, vect2, low, iPivot-1)
       Call quickSort(vect1, vect2, iPivot+1, high)
    End If       
  End Subroutine quickSort2vect

  Recursive Subroutine quickSort3vect(vect1, vect2, vect3, low, high)
    Implicit none
    Integer, Dimension(:), Intent(InOut) :: vect1, vect2
    Real(dp), Dimension(:), Intent(InOut) :: vect3
    Integer, Intent(In) :: low, high
    Integer i, iPivot, j
    iPivot = high    
    i = low
    Do While(iPivot > i)
       If (vect1(i) > vect1(iPivot)) then
          Call swap(vect1(i), vect1(iPivot-1))
          Call swap(vect2(i), vect2(iPivot-1))
          Call swap(vect3(i), vect3(iPivot-1))
          Call swap(vect1(iPivot-1), vect1(iPivot))
          Call swap(vect2(iPivot-1), vect2(iPivot))
          Call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       Else 
          i=i+1
       End If
    End Do
    If (low < high) then
       Call quickSort(vect1, vect2, vect3, low, iPivot-1)
       Call quickSort(vect1, vect2, vect3, iPivot+1, high)
    End If       
  End Subroutine quickSort3vect

  Recursive Subroutine quickSort4vect(vect1, vect2, vect3, vect4, low, high)
    Implicit none
    Real(dp), Dimension(:), Intent(InOut) :: vect1, vect2, vect3, vect4
    Integer, Intent(In) :: low, high
    Integer i, iPivot, j
    iPivot = high    
    i = low
    Do While(iPivot > i)
       If (vect1(i) > vect1(iPivot)) then
          Call swap(vect1(i), vect1(iPivot-1))
          Call swap(vect2(i), vect2(iPivot-1))
          Call swap(vect3(i), vect3(iPivot-1))
          Call swap(vect4(i), vect4(iPivot-1))
          Call swap(vect1(iPivot-1), vect1(iPivot))
          Call swap(vect2(iPivot-1), vect2(iPivot))
          Call swap(vect3(iPivot-1), vect3(iPivot))
          Call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       Else 
          i=i+1
       End If
    End Do
    If (low < high) then
       Call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       Call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    End If       
  End Subroutine quickSort4vect

  Subroutine swapInt(a,b)
    Implicit none
    Integer, Intent(InOut) :: a, b
    Integer ::  aux
    aux = b
    b = a
    a = aux
  End Subroutine swapInt
  Subroutine swapReal(a,b)
    Implicit none
    Real(dp), Intent(InOut) :: a, b
    Real(dp) ::  aux
    aux = b
    b = a
    a = aux
  End Subroutine swapReal

End Module quickSortMod
