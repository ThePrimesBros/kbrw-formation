    Why use an ETS table ?
    - Permet de stocker efficacement la donnée et de pouvoir la partager entre les processus
    What are the advantages of wrapping an ETS table inside a GenServer ?
    - Permet de créé un logique métier pour accéder au donnée de la table ets
    What is a Behaviour in Elixir ?
    - Ensemble de fonction qu'un module peut/doit implementer

Go further

    What are the differences between a Protocol and a Behaviour ?
    - Un behaviour est un ensemble de fonction api qu'un module doit implementer alors qu'un protocol est un ensemble d'implementation polymorphique qui permet d'ajouter/modifier des fonctions sans touché au code sources
    In which cases would you want to use a Protocol ? a Behaviour ?
    - On va utilisé les protocol lorsque l'on voudra travaillé avec les données pour modifier sans touché au code sources et sinon les behaviour
