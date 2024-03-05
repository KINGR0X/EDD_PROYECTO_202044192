module lista_clientes_atentidos
    implicit none
    private

    type, public ::node
        private
        integer :: value
        character(:), allocatable :: name_cliente
        integer :: img_g, img_p, total_pasos, num_ventanilla
        type(node), pointer :: next     
    end type node

    type, public :: lista_simple_atendidos
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
        procedure :: print_pointers
        procedure :: graficar
        procedure :: graficar_por_nombre
        procedure :: graficar_nodo_max_pasos
    end type lista_simple_atendidos

contains

    subroutine graficar_nodo_max_pasos(this, filename2)
        class(lista_simple_atendidos), intent(in) :: this
        character(len=*), intent(in) :: filename2
        type(node), pointer :: current, max_node
        integer :: max_pasos

        integer :: unit

        ! Inicializar el nodo máximo
        max_pasos = -1
        max_node => null()

        ! Encontrar el nodo con el máximo total_pasos
        current => this%head
        do while (associated(current))
            if (current%total_pasos > max_pasos) then
                max_pasos = current%total_pasos
                max_node => current
            end if
            current => current%next
        end do

        ! Graficar el nodo con el máximo total_pasos si se encontró uno
        if (associated(max_node)) then
            ! call graficar_por_nombre(this, filename2, max_node%name_cliente)

            open(unit, file=filename2, status='replace')
            write(unit, *) 'digraph lista_atendidos {'
            write(unit, *) 'label= "Reporte cliente";'
            write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];' ! Aplicar atributos a todos los nodos

            write(unit, *) '    "Node1', '" [label="',max_node%name_cliente, &
                "\n Img_g =",max_node%img_g,&
                "\n Img_p =",max_node%img_p,&
                "\n pasos =",max_node%total_pasos,&
                "\n ventanilla de atencion=",&
                max_node%num_ventanilla,'"];'

                ! Cerrar el archivo DOT
                write(unit, *) '}'
                close(unit)

                ! Generar el archivo PNG utilizando Graphviz
                call system('dot -Tpdf ' // trim(filename2) // ' -o ' // trim(adjustl(filename2)) // '.pdf')

                print *, 'Graphviz file generated: ', trim(adjustl(filename2)) // '.pdf'
        else
            print *, 'No se encontró ningún nodo en la lista.'
        end if
    end subroutine graficar_nodo_max_pasos

    subroutine graficar_por_nombre(this, filename, nombre_cliente)
        class(lista_simple_atendidos), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: nombre_cliente
        type(node), pointer :: current

        integer :: img, imp, pa, ven
        integer :: unit
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph lista_atendidos {'
        write(unit, *) 'label= "Reporte cliente";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];' ! Aplicar atributos a todos los nodos

        current => this%head
        count = 0

        do while (associated(current))
            if (trim(current%name_cliente) == trim(nombre_cliente)) then
                ! print *, 'Cliente encontrado: ', current%name_cliente
                count = count + 1
                img = current%img_g
                imp = current%img_p
                pa = current%total_pasos
                ven = current%num_ventanilla

                write(unit, *) '    "Node', count, '" [label="',current%name_cliente, &
                "\n Img_g =",img,"\n Img_p =",imp,&
                "\n pasos =",pa,"\n ventanilla de atencion=",ven,'"];'

            end if
            current => current%next
        end do

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)

        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'

    end subroutine graficar_por_nombre


    subroutine push(this, value)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        ! print *, 'pushed ', value
    end subroutine push

    subroutine append(this,value, name_cliente, num_ventanilla, img_g, img_p, total_pasos)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value
        character(len=*), intent(in) :: name_cliente
        integer, intent(in) :: img_g, img_p, total_pasos, num_ventanilla

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%value = value
        temp%name_cliente = name_cliente
        temp%num_ventanilla = num_ventanilla
        temp%img_g = img_g
        temp%img_p = img_p
        temp%total_pasos = total_pasos
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        ! print *, 'appended ', value
    end subroutine append

    subroutine delete(this, value)
        class(lista_simple_atendidos), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        current => this%head
        previous => null()

        ! Buscar el nodo a eliminar
        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        ! Si se encontró el nodo
        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            ! print *, 'Se eliminó el valor ', value
        else
            ! print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(lista_simple_atendidos), intent(in) :: this
        integer, intent(in) :: value

        type(node), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(lista_simple_atendidos), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Name Cliente:', current%name_cliente
            print *, 'Numero Ventanilla:', current%num_ventanilla
            print *, 'Imagen G:', current%img_g
            print *, 'Imagen P:', current%img_p
            print *, 'Total Pasos:', current%total_pasos
            print *, '-----------------'
            current => current%next
        end do 
    end subroutine print

    subroutine graficar(this,filename)
        class(lista_simple_atendidos), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current

        integer :: img, imp,pa,ven
        integer :: unit
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph lista_atendidos {'
        write(unit, *) 'label= "Lista clientes atendidos";'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=cornflowerblue];' ! Aplicar atributos a todos los nodos

        current => this%head
        count=0

        do while (associated(current))
            count = count + 1
            img= current%img_g
            imp= current%img_p
            pa= current%total_pasos
            ven= current%num_ventanilla

            write(unit, *) '    "Node', count, '" [label="',current%name_cliente, &
            "\n Img_g =",img,"\n Img_p =",imp,&
            "\n pasos =",pa,"\n ventanilla =",ven,'"];'

            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if

            ! print *, 'Index nodo:', current%value
            ! print *, 'Name Cliente:', current%name_cliente
            ! print *, 'Numero Ventanilla:', current%num_ventanilla
            ! print *, 'Imagen G:', current%img_g
            ! print *, 'Imagen P:', current%img_p
            ! print *, 'Total Pasos:', current%total_pasos
            current => current%next
        end do 

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'

    end subroutine graficar

    subroutine print_pointers(this)
        class(lista_simple_atendidos), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, 'Nodo actual: ', current%value, ', Nodo siguiente: ', current%next%value
            current => current%next
        end do 
    end subroutine print_pointers
    
end module lista_clientes_atentidos

! program main
!     use lista_clientes_atentidos
!     implicit none

!     type(lista_simple_atendidos) :: list

!     call list%append(1, "Cliente1", 1, 1, 1, 6)
!     call list%append(2, "Cliente2", 2, 2, 2, 5)
!     call list%append(3, "Cliente3", 3, 3, 3, 4)
!     ! call list%push(10)
!     ! call list%push(20)

!     print *, '//-----------------//'
!     print *, 'La lista es:'
!     print *, '//-----------------//'
!     call list%print()
!     call list%print_pointers()

!     ! print *, '//-----------------//'
!     ! if(list%search(1)) then
!     !     print *, 'El valor 1 esta en la lista'
!     ! else
!     !     print *, 'El valor 1 no esta en la lista'
!     ! end if
!     ! call list%delete(1)
!     ! if(list%search(1)) then
!     !     print *, 'El valor 1 esta en la lista'
!     ! else
!     !     print *, 'El valor 1 no esta en la lista'
!     ! end if

!     ! print *, '//-----------------//'
!     ! print *, 'La lista es:'
!     ! print *, '//-----------------//'
!     ! call list%print()

! end program main