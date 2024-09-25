#include <iostream>
#include <cmath>  // Para usar la función pow() para operaciones exponenciales 

int main() {
    // Primero definimos las variables necesarias
    double moles_acido, moles_base, moles_totales, peso_molar_acido, peso_molar_base, pH, pKa;

    // Pedir los valores necesarios como pH, pKa, y moles_totales;
    std::cout << "Dame el valor de pKa: ";
    std::cin >> pKa;

    std::cout << "Dame el valor de pH deseado: ";
    std::cin >> pH;

    std::cout << "Peso molar de ácido: ";
    std::cin >> peso_molar_acido;

    std::cout << "Peso molar de base: ";
    std::cin >> peso_molar_base;

    // Solicitar el valor de moles_totales
    std::cout << "Dame el número total de moles en la solución amortiguadora: ";
    std::cin >> moles_totales;

    // Ahora realizamos las operaciones de cálculo de moles de ácido y base para solución amortiguadora
    moles_acido = moles_totales / (pow(10, pH - pKa) + 1);
    std::cout << "Masa de ácido: " << moles_acido * peso_molar_acido << std::endl;

    moles_base = moles_totales - moles_acido;
    std::cout << "Masa de base: " << moles_base * peso_molar_base << std::endl;

    return 0;
}