# Manual tecnico

## Requisitos del sistema

### Sistemas operativos compatibles

- Windows 10,11

### Memporia Ram

- Minimo 4gb de Ram

### Lenguajes de programación

- Fortran

## Descripción general

La empresa "Pixel Print Studio" se dedica a la impresión de figuras de pixel art en distintos
tamaños y tipos de papel. Con el crecimiento constante de clientes, ha surgido la necesidad
de optimizar los procesos de recepción y producción. En este sentido, se solicita a un
estudiante de ingeniería en sistemas aplicar conocimientos en estructuras de datos para
mejorar la eficiencia operativa.

Deberás desarrollar una aplicación que utilice estructuras de datos y algoritmos para simular
los diversos procesos en la empresa "Pixel Print Studio". La aplicación deberá representar
visualmente las estructuras mediante bibliotecas compatibles, como Graphviz.

## Archivo main.f90

En este archivo es donde se lee el archivo Json proporcionado por el usuario,se procesa la información y se crean las diferentes estructuras de datos. A continuación se muestra una parte del codigo donde se declara el uso de las diferentes estructuras de datos.

```fortran
program main
    use json_module

        use cola_recepcion
        use List_of_list_vent
        use cola_impresion
        use lista_clientes_espera
        use lista_clientes_atentidos

        implicit none

        type(cola) :: cola_recep
        type(List_of_list) :: list_ventanillas

        !colas de impresion (impresoras)
        type(cola_im) :: cola_img_grande
        type(cola_im) :: cola_img_pequena

        !cola de espera
        type(lista_circular) :: lista_espera

        ! Lista simple clientes atendidos
        type(lista_simple_atendidos):: lista_c_atendidos
```

## cola_recpcion.f90

En este archivo se encuentra la estructura de datos cola, la cual es esta simplemente enlazada, en esta cola se almacenan los clientes que se encuentran en el archivo Json.

Extracto del archivo cola_recepcion.f90:

```fortran
module cola_recepcion
    implicit none
    private

    type, public :: node
        private
        character(:), allocatable :: value
        integer :: img_g, img_p
        type(node), pointer :: next
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: return_cliente_name
        procedure :: return_cliente_img_g
        procedure :: return_cliente_img_p
        procedure :: graficar
    end type cola

contains

    subroutine append(this, value, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: value
        integer, intent(in) :: img_g, img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%img_g = img_g
        temp%img_p = img_p
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%end => temp
        else
            this%end%next => temp
            this%end => temp
        end if

        ! print *, 'Append ', value
        ! print *, 'Append ', img_g
        ! print *, 'Append ', img_p
    end subroutine append

```

## listaS_ventanilla.f90

En este archivo se crea la lista de las vantanillas, esta es una lista simplemente enlazada, donde cada nodo contiene una pila que almacenará las imagenes que los clientes manden a imprimir. Esta es la estructura de datos más importante para el programa ya que de el depende que los clientes y las impresiones pasen adecuada a cada una de sus respectivas estructuras.

Extracto del archivo listaS_ventanilla.f90:

```fortran
module List_of_list_vent
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type :: node
        integer :: index
        integer :: num_imgG=0
        integer :: num_imgP=0
        integer :: num_imgG_original=0
        integer :: num_imgP_original=0
        character(:), allocatable :: name_client
        logical :: ocupado = .false.
        type(node), pointer :: next => null()
        type(sub_node), pointer :: stack => null() ! pila

    contains
        procedure :: append
        procedure :: push
        procedure :: pop
        procedure :: print
        procedure :: pop_normal
    end type node

    type, public :: List_of_list
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: insert
        procedure :: printList
        procedure :: printPunteros
        procedure :: popAtIndex
        procedure :: asignar_datos_cliente
        procedure :: buscar_nodo_desocupado
        procedure :: imagenes_a_ventanilla
        procedure :: check_stack_size
        procedure :: verificar_nodo
        procedure :: pop_inicial
        procedure :: return_nombre_cliente
        procedure :: return_num_ventanilla
        procedure :: return_num_imgG
        procedure :: return_num_imgP
        procedure :: graficar
    end type List_of_list

contains

    function return_nombre_cliente(self, index) result(name_client)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        character(len=:), allocatable :: name_client

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                name_client = current%name_client
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end function return_nombre_cliente

    function return_num_ventanilla(self, index) result(num_ventanilla)
        class(List_of_list), intent(in) :: self
        integer, intent(in) :: index
        integer :: num_ventanilla

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%index == index) then
                num_ventanilla = current%index
                return
            end if
            current => current%next
        end do

        print *, 'El índice especificado no se encontró en la lista.'
    end function return_num_ventanilla
```

## cola_impresion.f90

Como su nombre indica en este archivo se encuentra la estructura de datos de una cola, de esta estrctura de datos se crean dos colas, una para las impresiones grandes y otra para las pequeñas.

Extracto del archivo cola_impresion.f90:

```fortran
module cola_impresion
    implicit none
    private

    type, public :: node_im
        private
        character(:), allocatable :: value
        type(node_im), pointer :: next
    end type node_im

    type, public :: cola_im
        private
        type(node_im), pointer :: head => null()
        type(node_im), pointer :: end => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: size_cola
        procedure :: graficar
    end type cola_im

contains
    subroutine graficar(this,filename,tipo_impresora)
        class(cola_im), intent(in) :: this
        type(node_im), pointer :: current
        character(len=*), intent(in) :: filename, tipo_impresora

        integer :: unit
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Cola_impresoras {'
        write(unit, *) 'label= "Cola de impresoras";'
        write(unit, *) '    node [shape=box, style=filled, color=black, fillcolor=mediumpurple];'

        current => this%head
        count = 0


        write(unit, *) '    "Node', count, '" [label="',tipo_impresora,'"];'

        do while (associated(current))
            count = count + 1

            if (count==1) then
                write(unit, *) '    "Node', count, '" [label="', current%value,'"];'
                write(unit, *) '    "Node', count, '" -> "Node', 0, '";'
            else
                write(unit, *) '    "Node', count, '" [label="', current%value,'"];'
                write(unit, *) '    "Node', count, '" -> "Node', count-1, '";'

            end if

            current => current%next
        end do

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)

        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpdf ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.pdf')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.pdf'

    end subroutine graficar
```

## lista_lista_espera.f90

Esta estructura de datos es una lista doblemente enlazada, y es una lista de listas ya que cada nodo contiene una lista simplemente enlazada que contiene las imagenes impresas de cada cliente.

Extracto del archivo lista_lista_espera.f90:

```fortran
module lista_clientes_espera
    implicit none
    private

    type :: sub_node
        character(:), allocatable :: value
        type(sub_node), pointer :: next => null()
    end type sub_node

    type, public :: node
        private
        integer :: value
        character(:), allocatable :: name_cliente
        integer :: img_g, img_p, total_pasos, num_ventanilla
        integer :: n_imgG_original, n_imgP_original
        type(sub_node), pointer :: stack => null() ! Pila asociada al nodo
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()

    contains
        procedure :: append_to_stack

    end type node

    ! Lista circular
    type, public :: lista_circular
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: append_img
        procedure :: get_img_g
        procedure :: get_img_p
        procedure :: get_size
        procedure :: get_name_cliente
        procedure :: get_num_ventanilla
        procedure :: get_total_pasos
        procedure :: verificar_completo
        procedure :: get_min_value
        procedure :: graficar
        procedure :: get_stack_size
        procedure :: print_pointers
    end type lista_circular

contains

    subroutine print_pointers(self)
        class(lista_circular), intent(inout) :: self
        type(node), pointer :: current

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head

        do while (associated(current))
            print *, 'Index nodo:', current%value
            print *, 'Prev:', current%prev%value
            print *, 'Next:', current%next%value
            print *, ""

            if (associated(current%next, self%head)) exit  ! Salir al completar un ciclo (lista circular)
            current => current%next
        end do
    end subroutine print_pointers


    function get_min_value(self) result(min_value)
        class(lista_circular), intent(in) :: self
        integer :: min_value
        type(node), pointer :: current
        min_value = 0 ! Valor inicial

        if (.not. associated(self%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => self%head
        min_value = current%value

        do
            current => current%next
            if (current%value < min_value) then
                min_value = current%value
            end if
            if (associated(current, self%head)) exit
        end do
    end function get_min_value


    function get_total_pasos(self, index) result(total_pasos)
        class(lista_circular), intent(in) :: self
        integer, intent(in) :: index
        integer :: total_pasos

        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            if (current%value == index) then
                total_pasos = current%total_pasos
                return
            end if
            current => current%next
        end do

        ! print *, "El índice especificado no se encontró en la lista."
    end function get_total_pasos
```

## lista_csimple.f90

Esta lista como su nombre indica es una lista simplemente enlazada, en esta lista se almacenan los clientes que ya han sido atendidos. Cada nodo contiene el nombre del cliente, cantidad de impresiones grandes, cantidad de imagenes pequeñas, el total de pasos que tardo en ser atendido, y el número de ventanilla que lo atendio.

Extracto del archivo lista_csimple.f90:

```fortran
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
```

Cada estructura tiene diferentes metodos y fuinciones necesarias para el correcto funcionamiento del programa, cada una de estas estructuras de datos se encuentran en archivos separados para una mejor organización del código.

Algunas de las funciones comunes entre cada estructura pero de diferente implementación son:
append, print, graficar, delete, entre otras.
