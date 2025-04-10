/**
 * Ejercicio 1
 */

// print("Hola, mundo")

// Exp. 1 y 2
/*
let miFloat: Float = 4
var miEtiqueta = "El valor es "

var miEtiqueta2 = miEtiqueta

// cannot convert value of type 'Float' to expected argument Al hacerlo sin String
miEtiqueta = miEtiqueta + String(miFloat) + " " + miEtiqueta  // + = append
//miEtiqueta.append(miFloat)

//++miEtiqueta // da error
//miEtiqueta = miEtiqueta - miEtiqueta // da error
print("1 |", miEtiqueta)

miEtiqueta2 = "\(miEtiqueta2)\(miFloat)"
print("2 |", miEtiqueta2)
*/

// Exp. 3
/*
let nameSalute = "internauta"
let miResult: Float = 9 / 2

print("Resultado: \(miResult * 2) = 9")
print("¡Saludos, \(nameSalute)!")
*/

// Exp. 4
/*
var cadenaOpcional: String? = "Hola"
print(cadenaOpcional == nil)

var nombreOpcional: String? = nil
var saludo = "Hola!"
if let nombre = nombreOpcional {
  saludo = "Hola, \(nombre)"
} else {
  saludo += saludo  // si es nil, sale esto
}
print(saludo)
*/

// Exp. 5
/*
let nombrePila: String? = nil
let nombreCompleto: String = "John Appleseed"
let saludoInformal = "¿Qué tal, \(nombrePila == nil ? "undefined" : nombreCompleto)?"
print(saludoInformal)

let verdura = "zanahoria"
switch verdura {  // Switch must be exhaustive
case "zanahoria":
  print("Buena para la vista.")
case "lechuga", "tomates":
  print("Podrías hacer una buena ensalada.")
default:
  print("Siempre puedes hacer una buena sopa.")
}
print("Hola desde casa")
*/

// Exp. 6
