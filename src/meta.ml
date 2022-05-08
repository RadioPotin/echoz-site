let keywords_l =
  [ "echo'z"
  ; "echo-z"
  ; "eco"
  ; "z"
  ; "echo"
  ; "association"
  ; "seine-et-marne"
  ; "musique"
  ; "ecologie"
  ; "circuits courts"
  ; "festival"
  ; "zamal"
  ; "culture"
  ; "champs"
  ]

let default_keywords = Dream.html_escape (String.concat ", " keywords_l)

let default_description =
  {|Echo'Z est une assiociation Française qui veut faire le pont entre les problématiques écologiques de notre temps, l'accès à la culture, la musique et les liens entre les humains. Nous sommes basés dans le Sud de la Seine-et-Marne, et nous organisons depuis 2019 le festival Le Burning Zamal.|}
