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

Balance:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && ./simulate.exe 5 12 2 6 6
Eksploder:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && ./simulate.exe 10 12 1 3 6
Forsvinder:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll simulate.fsx && ./simulate.exe 10 12 16 3 97



For at kører white-box testing skal der skrives:
  fsharpc -a preditorPrey.fs && fsharpc --nologo -r preditorPrey.dll testPreditorPrey.fsx && testPreditorPrey.exe
