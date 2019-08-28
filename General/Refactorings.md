# Refactoring in Pharo 8.0

## Introduction

Pharo offers many automatic refactoring tools. Therefore, the objective of this tutorial is to present what refactorings are, when to apply them, how to apply them and to show any possible inconvenience before using them.

## What is refactoring?

Martin Fowler defines refactoring as: "It is a disciplined technique to restructure an existing body of code, altering its internal structure without changing its external behavior." This means that before carrying out any type of refactoring in the code it is important to have complete test coverage to demonstrate that the code behavior does not change during the refactoring process.

## The most popular refactorings supported by Pharo 8.0

| Refactoring | Supported in |
| ---------- | ---------- |
| Rename   | Package, Class, Method, Instance and Temporary Variable |
|  Extract   | Method and Temporary Variable  |
| Remove| Package, Class, Method and Instance Variable |
| Copy | Class |
| Move | Class and method |
| Inline | Method and Temporary Variable |
| Push up / Push down | Method and Instance Variable |
| Replace | Method |
| Other refactorings|  Class, Method, Instance and Temporary Variable |

# Rename

Renaming seems like a trivial refactoring however it can lead to many complications because this action can lead to involuntary changes of elements not related to the same name. The Pharo's rename tool has can minimize these errors.

## When should we rename?

1. When the name is not descriptive enough
2. The name of the class / method / variable does not match what it really is.
3. Something new was introduced that requires the existing code to have a new more specific name.

> This refactoring can be done for classes, methods and variables.

> 1. Select the class / variable / method 
> 2. Press *⌘R* or from the menu, select **Rename**.
> 3. Fill the input of dialog with the new name and press **Ok** button.

It should be noted that the renowned methods have some options more than classes and variables.

# Extract

The extraction Pharo's tools allow developers to reshape their code when they have duplicate code or when they want to change the design.

> Pharo can perform this refactoring to extract methods and extract temporal variables.

> **Extraer variable temporal**

Antes

Example >> method
    | a b c|
    a := 1.
    b := a + list size.
    c := b + list size.

Despues

Example >> method
    | a b c d|
    a := 1.
    d := list size.
    b := a + d.
    c := b + d.

1. Seleccionar la seccion del codigo fuente que desea convertir
en variable temporal.
2. Press ⌘T or from the menu, select Source code.
3. Select extract temp option .
4. Fill the input of dialog with the name of the variable and press
Ok button.

Extraer metodo

Antes

Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := a + b.
    d := a + c.

Despues

Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := self add: a to: b.
    d := self add: a to: c.
Example >> add: a to: b
    ^ a + b

1. Seleccionar la seccion del codigo fuente que desea extraer.
2. Press ⌘T or from the menu, select Source code.
3. Select extract method option.
4. Fill the input of dialog with the name of method and press
Ok button.

Eliminacion

A veces cuando se refactoriza el codigo puede terminar con codigo que 
ya no se usa, o que ya no deberia usarse; por lo cual es necesario 
eliminar todo aquel codigo no utilizado ya que esto afecta a los desarrolladores
que trabajan e intentan entender el codigo fuente.

Procedimiento

Esta refactorizacion se puede hacer tanto para paquetes, clases, metodos y variables.
A continuacion se especifican los pasos a seguir

1. Seleccionar el paquete / clase / variable / metodo 
2. Press ⌘X

Copiar

Muchas veces queremos cambiar o agregar algunas funcionalidades a nuestro codigo,
sin embargo esto conlleva grandes cambios lo cual hace que sea peligroso realizarlo sobre la
clase orginal. Es por ello que puede resultar necesario copiar las clases
para tener un respaldo en caso de que se quiera revertir los cambios.

Procedimiento

Actualmente esta herramienta solo se aplica a clases y para utilizarla solo debe seguir los siguientes pasos:

1. Seleccionar la clase
2. Press ⌘C or from the menu, select Copy.

Mover

La refactorizacion mover permite mover clases y metodos de un paquete
a otro, y mover metodos al lado de la clase / instancia o incluso
moverlos a otra clase.

Procedimiento

Move to package

Class

1. Seleccionar la clases o clases que se desea mover.
2. Press ⌘MC or from the menu, select Refactorings and then select 
Move to package option.
3. Select the package and press Ok button

Method

1. Seleccionar el o los metodos que se desea mover.
2. Select Refactorings and then select Move to package option.
3. Select the package and press Ok button

Move to class

1. Seleccionar el o los metodos que se desea mover.
2. Select Refactorings and then select Move to another class option.
3. Select the new class for method(s) and press Ok button

Move to class side

1. Seleccionar el o los metodos que se desea mover.
2. Press ⌘TC, select Refactorings and then select Move to class side option.

Move to instance side

1. Seleccionar el o los metodos que se desea mover.
2. Press ⌘TI, select Refactorings and then select Move to instance side option.

Inline

Inline refactoring permite revertir la refactorizacion de extraccion
de un metodo o variable temporal.

Procedimiento

Inline temp

La refactorización de variables en línea reemplaza el uso de variables redundantes con su inicializador.

Antes

MyA >> method
    | number b |
    number := anotherClass value.
    b := 3 + number.

Despues

MyA >> method
    | b |
    b := 3 + anotherClass value.

1. Select the temporary variable in the source code
2. Press ⌘T or from the menu, select Source code.
3. Select Inline temp option.

Inline method

Los resultados del método en línea colocan el cuerpo del método en el 
cuerpo del metodo donde seleccionamos su llamada.

Antes

Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := self add: a to: b.
    d := self add: a to: c.

Example >> add: a to: b
    ^ a + b

Despues

Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := a + b.
    d := a + c.

1. Select the call to method in the source code
2. Press ⌘T or from the menu, select Source code.
3. Select Inline method option.

Inline senders

Esta refactorizacion reemplaza todas las llamadas al metodo seleccionado
desde su misma clase por el cuerpo de su implementacion ademas de que 
el metodo en si sera eliminado.

1. Select the method
2. From the menu, select Refactorings.
3. Select Inline senders option.

Push up and Push down

Pull up method

Gets rid of duplicate code. If you need to make changes to a method,
 it’s better to do so in a single place than have to search for all 
 duplicates of the method in subclasses; this refactoring technique 
 can also be used if, for some reason, a subclass redefines a 
 superclass method but performs what’s essentially the same work.

Procedimiento

1. Select the method
2. Select ⌘PU or from the menu, select Refactorings and then select 
Push up option.

Push down method

Improves class coherence. A method is located where you expect to
 see it. For example if you see that a method is needed by more than
  one subclass, but not all of them, it may be useful to create an 
  intermediate subclass and move the method to it. This allows 
  avoiding the code duplication that would result from pushing a
   method down to all subclasses.

Procedimiento

1. Select the method
2. Select ⌘PD or from the menu, select Refactorings and then select 
Push down option.

De la misma manera que en los metodos se puede realizar
las refactorizaciones push up and push down sobre las variables de instancia

Push up variable

1. Select the variable
2. From the menu select Push up option.

Push down variable

1. Select the variable
2. From the menu select Push down option.

Replace

Replace senders

Esta refactorizacion nos sirve para cambiar los senders del metodo
seleccionado al nombre de otro metodo que nosotros querramos,
considerando que el metodo con el que lo reemplacemos debe tener
la misma cantidad de argumentos que el original.

Procedimiento

1. Select the method
2. From the menu, select Refactorings.
3. Select Replace senders option.

Find and Replace method

Esta refactorizacion nos ayuda cuando tenemos codigo duplicado, en si
esta refactorizacion consiste en seleccionar un metodo y buscar las 
ocurrencias de su cuerpo en un rango de metodos que se selecciona.

Antes

Example >> textInputOn: html withName: aString andSymbol: aSymbol
	html text: aString.
	html textInput on: aSymbol of: self contact.
	html break

Example >> renderContentOn: html
	html
		form: [ html text: 'Name:'.
			html textInput on: #name of: self contact.
			html break.
			html text: 'Email address:'.
			html textInput on: #emailAddress of: self contact.
			html break.
			html text: 'Example:'.
			html textInput on: #example of: self contact.
			html break]

Despues

Example >> textInputOn: html withName: aString andSymbol: aSymbol
	html text: aString.
	html textInput on: aSymbol of: self contact.
	html break

Example >> renderContentOn: html
	html
		form: [ self textInputOn: html withName: 'Name:' andSymbol: #name.
			self textInputextInputOnt: html withName: 'Email address:' andSymbol: #emailAddress.
			self textInputOn: html withName: 'Example:' andSymbol: #example ]

Procedimiento

1. Select the method
2. From the menu, select Refactorings.
3. Select Find and replace option.

Otras refactorizaciones

Generate accessors

Esta refactorizacion genera los mensajes get y set de las variables de instancia. Se puede
usar esta refactorizacion desde la clase y las variables.

Before

Object subclass: #Example
	instanceVariableNames: 'a'
	classVariableNames: ''
	package: 'Refactoring-Example'

After

Object subclass: #Example
	instanceVariableNames: 'a'
	classVariableNames: ''
	package: 'Refactoring-Example'

Example >> a
    ^ a

Example >> a: anObject
    a := anObject

Class

1. Select the class that you want generate accessors
2. Select ⌘GA or from the menu, select Generate accessors option

Variables

1. Select the variable(s) that you want generate accessors
2. From the menu, select Generate accessors option

Insert subclass



Insert superclass



New subclass

