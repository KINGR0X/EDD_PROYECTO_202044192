program BTree
    implicit none

    ! Order 4
    integer, parameter :: MAXI = 3, MINI = 1 

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr
    
    type BTreeNode
        integer :: val(0:MAXI+1)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
    end type BTreeNode

    type(BTreeNode), pointer :: root => null()
    	
    call insert(5)
    call insert(10)
    call insert(15)
    call insert(20)
    call insert(25)
    call insert(30)
    call insert(35)
    call insert(40)
    call insert(45)
    call insert(46)
    call insert (11)
    call traversal(root)
    print * , ''

    call generar_grafica(root, 'btree.dot')
    	
contains

    subroutine insert(val)
            integer, intent(in) :: val
            integer :: i
            type(BTreeNode), pointer :: child
            allocate(child)
            if (setValue(val, i, root, child)) then
                    root => createNode(i, child)
            end if
    end subroutine insert

    recursive function setValue(val, pval, node, child) result(res)
            integer, intent(in) :: val
            integer, intent(inout) :: pval
            type(BTreeNode), pointer, intent(inout) :: node
            type(BTreeNode), pointer, intent(inout) :: child
            type(BTreeNode), pointer :: newnode        
            integer :: pos
            logical :: res
            allocate(newnode)
            if (.not. associated(node)) then            
                    pval = val
                    child => null()
                    res = .true.
                    return
            end if
            if (val < node%val(1)) then
                    pos = 0
            else
                    pos = node%num
                    do while (val < node%val(pos) .and. pos > 1) 
                    pos = pos - 1
                    end do
                    if (val == node%val(pos)) then
                        print *, "Duplicates are not permitted"
                        res = .false.
                        return
                    end if
            end if
            if (setValue(val, pval, node%link(pos)%ptr, child)) then
                    if (node%num < MAXI) then
                        call insertNode(pval, pos, node, child)
                    else
                        call splitNode(pval, pval, pos, node, child, newnode)
                        child => newnode
                        res = .true.
                    return
                end if
            end if
            res = .false.
    end function setValue

    subroutine insertNode(val, pos, node, child)
            integer, intent(in) :: val, pos
            type(BTreeNode), pointer, intent(inout) :: node
            type(BTreeNode), pointer, intent(in) :: child
            integer :: j
            j = node%num
            do while (j > pos)
                    node%val(j + 1) = node%val(j)
                    node%link(j + 1)%ptr => node%link(j)%ptr
                    j = j - 1
            end do
            node%val(j + 1) = val
            node%link(j + 1)%ptr => child
            node%num = node%num + 1
        end subroutine insertNode

    subroutine splitNode(val, pval, pos, node, child, newnode)
            integer, intent(in) :: val, pos
            integer, intent(inout) :: pval
            type(BTreeNode), pointer, intent(inout) :: node,  newnode
            type(BTreeNode), pointer, intent(in) ::  child
            integer :: median, i, j
            if (pos > MINI) then
                    median = MINI + 1
            else
                    median = MINI
            end if
            if (.not. associated(newnode)) then
                allocate(newnode)
            do i = 0, MAXI
                        newnode%link(i)%ptr => null()
                enddo
            end if
            j = median + 1
            do while (j <= MAXI)
                    newnode%val(j - median) = node%val(j)
                    newnode%link(j - median)%ptr => node%link(j)%ptr
                    j = j + 1
            end do
            node%num = median
            newnode%num = MAXI - median
            if (pos <= MINI) then
                    call insertNode(val, pos, node, child)
            else
                    call insertNode(val, pos - median, newnode, child)
            end if        
            pval = node%val(node%num)        
            newnode%link(0)%ptr => node%link(node%num)%ptr
            node%num = node%num - 1
    end subroutine splitNode

    function createNode(val, child) result(newNode)
            integer, intent(in) :: val
            type(BTreeNode), pointer, intent(in) :: child
            type(BTreeNode), pointer :: newNode
            integer :: i
            allocate(newNode)
            newNode%val(1) = val
            newNode%num = 1
            newNode%link(0)%ptr => root
            newNode%link(1)%ptr => child
            do i = 2, MAXI
                    newNode%link(i)%ptr => null()
            end do
    end function createNode

    recursive subroutine traversal(myNode)
            type(BTreeNode), pointer, intent(in) :: myNode
            integer :: i
            if (associated(myNode)) then
                    write (*, '(A)', advance='no') ' [ '
                    i = 0
                    do while (i < myNode%num)
                        ! Escribe el valor del nodo sin saltar linea
                        write (*,'(1I3)', advance='no') myNode%val(i+1)
                        i = i + 1
                    end do
                    do i = 0, myNode%num
                        ! Llama a la funcion de forma recursiva
                        call traversal(myNode%link(i)%ptr)    
                    end do
                    write (*, '(A)', advance='no') ' ] '
            end if
    end subroutine traversal






    recursive subroutine graph(myNode,unit)
        type(BTreeNode), pointer, intent(in) :: myNode
        integer :: i
        integer :: unit

        if (associated(myNode)) then
                i = 0
                do while (i < myNode%num)
                        ! Escribe el valor del nodo sin saltar linea
                        write(unit, *) '"Node', myNode%val(i+1), '" [label="',myNode%val(i+1), '"]'

                        i = i + 1
                        
                end do
                do i = 0, myNode%num
                        ! Llama a la funcion de forma recursiva
                        call graph(myNode%link(i)%ptr,unit) 
                end do
        end if

    end subroutine graph

        subroutine generar_grafica(self,filename)
                type(BTreeNode), pointer, intent(in) :: self
                character(len=*), intent(in) :: filename
                integer :: unit
                integer :: i
                character(len=100) :: dot_filename, dot_command, png_filename

                ! Agregar extensiones
                dot_filename = trim(filename) // ".dot"
                png_filename = trim(filename) // ".png"

                ! Construir comandos para generar la imagen
                dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)

                unit=1
                open(newunit=unit, file=trim(dot_filename))
                write(unit, *) 'digraph {'
                write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];' ! Aplicar atributos a todos los nodos
                
                if (associated(self)) then
                        call graph(self,unit)
                end if

                ! Cerrar el archivo DOT
                write(unit, *) '}'
                close(unit)

                ! Ejecutar el comando para generar la imagen
                call execute_command_line(trim(dot_command), exitstat=i)

                ! Comprobar si hubo algún error al generar la imagen
                if (i /= 0) then
                print *, "Ocurrió un error al momento de crear la imagen"
                else
                print *, "La imagen fue generada exitosamente"
                end if

        end subroutine generar_grafica



end program BTree
