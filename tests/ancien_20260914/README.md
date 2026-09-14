Instantanés datés des deux scripts d'entrée AVANT le chantier « orchestration par étapes »
(commit c2a7e0a, 2026-09-14). Utilisés UNIQUEMENT par tests/test_chaines_sqlite.R comme
référence d'identité : le nouveau flux par étapes doit produire, sur les mêmes fixtures et le
même seed, des sorties identiques bit à bit à celles de ces scripts (qui consomment la
config et les helpers courants). Ne pas exécuter en production.
