For at køre filerne, åbnes mono kommandoprompten, hvori vi går ind i "/src" mappen.
Herfra kan koden oversættes ved at skrive:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx
Ellers kan koden både oversættes og køres direkte ved at kører:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && simulate.exe <parametre>
f.eks.:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && simulate.exe 10 12 3 3 2

Nu kan vi køre vores .exe fil, ved at skrive:
  simulate.exe <parametre>

På linux skal der huskes at gives execute permissions:
`chmod u+x <program-fil>`
Hvis det køres på linux skal der skrives:
./simulate.exe <>

Eksploder:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && ./simulate.exe 15 20 3 6 6
Balance:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && ./simulate.exe 6 12 3 3 6

