
(defvar test-data-first-names-male
  '("James" "John" "Robert" "Michael" "William" "David" "Richard"
  "Charles" "Joseph" "Thomas" "Christopher" "Daniel" "Paul" "Mark" "Donald"
  "George" "Kenneth" "Steven" "Edward" "Brian" "Ronald" "Anthony"
  "Kevin" "Jason" "Matthew" "Gary" "Timothy" "Jose" "Larry" "Jeffrey"
  "Frank" "Scott" "Eric" "Stephen" "Andrew" "Raymond" "Gregory"
  "Joshua" "Jerry" "Dennis" "Walter" "Patrick" "Peter" "Harold"
  "Douglas" "Henry" "Carl" "Arthur" "Ryan" "Roger" "Joe" "Juan" "Jack"
  "Albert" "Jonathan" "Justin" "Terry" "Gerald" "Keith" "Samuel"
  "Willie" "Ralph" "Lawrence" "Nicholas" "Roy" "Benjamin" "Bruce"
  "Brandon" "Adam" "Harry" "Fred" "Wayne" "Billy" "Steve" "Louis"
  "Jeremy" "Aaron" "Randy" "Howard" "Eugene" "Carlos" "Russell"
  "Bobby" "Victor" "Martin" "Ernest" "Phillip" "Todd" "Jesse" "Craig"
  "Alan" "Shawn" "Clarence" "Sean" "Philip" "Chris" "Johnny" "Earl"
  "Jimmy" "Antonio" "Danny" "Bryan" "Tony" "Luis" "Mike" "Stanley"
  "Leonard" "Nathan" "Dale" "Manuel" "Rodney" "Curtis" "Norman"
  "Allen" "Marvin" "Vincent" "Glenn" "Jeffery" "Travis" "Jeff" "Chad"
  "Jacob" "Lee" "Melvin" "Alfred" "Kyle" "Francis" "Bradley" "Jesus"
  "Herbert" "Frederick" "Ray" "Joel" "Edwin" "Don" "Eddie" "Ricky"
  "Troy" "Randall" "Barry" "Alexander" "Bernard" "Mario" "Leroy"
  "Francisco" "Marcus" "Micheal" "Theodore" "Clifford" "Miguel"
  "Oscar" "Jay" "Jim" "Tom" "Calvin" "Alex" "Jon" "Ronnie" "Bill"
  "Lloyd" "Tommy" "Leon" "Derek" "Warren" "Darrell" "Jerome" "Floyd"
  "Leo" "Alvin" "Tim" "Wesley" "Gordon" "Dean" "Greg" "Jorge" "Dustin"
  "Pedro" "Derrick" "Dan" "Lewis" "Zachary" "Corey" "Herman" "Maurice"
  "Vernon" "Roberto" "Clyde" "Glen" "Hector" "Shane" "Ricardo" "Sam"
  "Rick" "Lester" "Brent" "Ramon" "Charlie" "Tyler" "Gilbert" "Gene"
  "Marc" "Reginald" "Ruben" "Brett" "Angel" "Nathaniel" "Rafael"
  "Leslie" "Edgar" "Milton" "Raul" "Ben" "Chester" "Cecil" "Duane"
  "Franklin" "Andre" "Elmer" "Brad" "Gabriel" "Ron" "Mitchell"
  "Roland" "Arnold" "Harvey" "Jared" "Adrian" "Karl" "Cory" "Claude"
  "Erik" "Darryl" "Jamie" "Neil" "Jessie" "Christian" "Javier"
  "Fernando" "Clinton" "Ted" "Mathew" "Tyrone" "Darren" "Lonnie"
  "Lance" "Cody" "Julio" "Kelly" "Kurt" "Allan" "Nelson" "Guy"
  "Clayton" "Hugh" "Max" "Dwayne" "Dwight" "Armando" "Felix" "Jimmie"
  "Everett" "Jordan" "Ian" "Wallace" "Ken" "Bob" "Jaime" "Casey"
  "Alfredo" "Alberto" "Dave" "Ivan" "Johnnie" "Sidney" "Byron"
  "Julian" "Isaac" "Morris" "Clifton" "Willard" "Daryl" "Ross"
  "Virgil" "Andy" "Marshall" "Salvador" "Perry" "Kirk" "Sergio"
  "Marion" "Tracy" "Seth" "Kent" "Terrance" "Rene" "Eduardo"
  "Terrence" "Enrique" "Freddie" "Wade"))

(defvar test-data-first-names-female
  '("Mary" "Patricia" "Linda" "Barbara" "Elizabeth" "Jennifer" "Maria"
  "Susan" "Margaret" "Dorothy" "Lisa" "Nancy" "Karen" "Betty" "Helen"
  "Sandra" "Donna" "Carol" "Ruth" "Sharon" "Michelle" "Laura" "Sarah"
  "Kimberly" "Deborah" "Jessica" "Shirley" "Cynthia" "Angela"
  "Melissa" "Brenda" "Amy" "Anna" "Rebecca" "Virginia" "Kathleen"
  "Pamela" "Martha" "Debra" "Amanda" "Stephanie" "Carolyn" "Christine"
  "Marie" "Janet" "Catherine" "Frances" "Ann" "Joyce" "Diane" "Alice"
  "Julie" "Heather" "Teresa" "Doris" "Gloria" "Evelyn" "Jean" "Cheryl"
  "Mildred" "Katherine" "Joan" "Ashley" "Judith" "Rose" "Janice"
  "Kelly" "Nicole" "Judy" "Christina" "Kathy" "Theresa" "Beverly"
  "Denise" "Tammy" "Irene" "Jane" "Lori" "Rachel" "Marilyn" "Andrea"
  "Kathryn" "Louise" "Sara" "Anne" "Jacqueline" "Wanda" "Bonnie"
  "Julia" "Ruby" "Lois" "Tina" "Phyllis" "Norma" "Paula" "Diana"
  "Annie" "Lillian" "Emily" "Robin" "Peggy" "Crystal" "Gladys" "Rita"
  "Dawn" "Connie" "Florence" "Tracy" "Edna" "Tiffany" "Carmen" "Rosa"
  "Cindy" "Grace" "Wendy" "Victoria" "Edith" "Kim" "Sherry" "Sylvia"
  "Josephine" "Thelma" "Shannon" "Sheila" "Ethel" "Ellen" "Elaine"
  "Marjorie" "Carrie" "Charlotte" "Monica" "Esther" "Pauline" "Emma"
  "Juanita" "Anita" "Rhonda" "Hazel" "Amber" "Eva" "Debbie" "April"
  "Leslie" "Clara" "Lucille" "Jamie" "Joanne" "Eleanor" "Valerie"
  "Danielle" "Megan" "Alicia" "Suzanne" "Michele" "Gail" "Bertha"
  "Darlene" "Veronica" "Jill" "Erin" "Geraldine" "Lauren" "Cathy"
  "Joann" "Lorraine" "Lynn" "Sally" "Regina" "Erica" "Beatrice"
  "Dolores" "Bernice" "Audrey" "Yvonne" "Annette" "June" "Samantha"
  "Marion" "Dana" "Stacy" "Ana" "Renee" "Ida" "Vivian" "Roberta"
  "Holly" "Brittany" "Melanie" "Loretta" "Yolanda" "Jeanette" "Laurie"
  "Katie" "Kristen" "Vanessa" "Alma" "Sue" "Elsie" "Beth" "Jeanne"
  "Vicki" "Carla" "Tara" "Rosemary" "Eileen" "Terri" "Gertrude" "Lucy"
  "Tonya" "Ella" "Stacey" "Wilma" "Gina" "Kristin" "Jessie" "Natalie"
  "Agnes" "Vera" "Willie" "Charlene" "Bessie" "Delores" "Melinda"
  "Pearl" "Arlene" "Maureen" "Colleen" "Allison" "Tamara" "Joy"
  "Georgia" "Constance" "Lillie" "Claudia" "Jackie" "Marcia" "Tanya"
  "Nellie" "Minnie" "Marlene" "Heidi" "Glenda" "Lydia" "Viola"
  "Courtney" "Marian" "Stella" "Caroline" "Dora" "Jo" "Vickie"

  "Mattie" "Terry" "Maxine" "Irma" "Mabel" "Marsha" "Myrtle" "Lena"
  "Christy" "Deanna" "Patsy" "Hilda" "Gwendolyn" "Jennie" "Nora"
  "Margie" "Nina" "Cassandra" "Leah" "Penny" "Kay" "Priscilla" "Naomi"
  "Carole" "Brandy" "Olga" "Billie" "Dianne" "Tracey" "Leona" "Jenny"
  "Felicia" "Sonia" "Miriam" "Velma" "Becky" "Bobbie" "Violet"
  "Kristina" "Toni" "Misty" "Mae" "Shelly" "Daisy" "Ramona" "Sherri"
  "Erika" "Katrina" "Claire" "Lindsey" "Lindsay" "Geneva" "Guadalupe"
  "Belinda" "Margarita" "Sheryl" "Cora" "Faye" "Ada" "Natasha"
  "Sabrina" "Isabel" "Marguerite" "Hattie" "Harriet" "Molly" "Cecilia"
  "Kristi" "Brandi" "Blanche" "Sandy" "Rosie" "Joanna" "Iris" "Eunice"
  "Angie" "Inez" "Lynda" "Madeline" "Amelia" "Alberta" "Genevieve"
  "Monique" "Jodi" "Janie" "Maggie" "Kayla" "Sonya" "Jan" "Lee"
  "Kristine" "Candace" "Fannie" "Maryann" "Opal" "Alison" "Yvette"
  "Melody" "Luz" "Susie" "Olivia" "Flora" "Shelley" "Kristy" "Mamie"
  "Lula" "Lola" "Verna" "Beulah" "Antoinette" "Candice" "Juana"
  "Jeannette" "Pam" "Kelli" "Hannah" "Whitney" "Bridget" "Karla"
  "Celia" "Latoya" "Patty" "Shelia" "Gayle" "Della" "Vicky" "Lynne"
  "Sheri" "Marianne" "Kara" "Jacquelyn" "Erma" "Blanca" "Myra"
  "Leticia" "Pat" "Krista" "Roxanne" "Angelica" "Johnnie" "Robyn"
  "Francis" "Adrienne" "Rosalie" "Alexandra" "Brooke" "Bethany"
  "Sadie" "Bernadette" "Traci" "Jody" "Kendra" "Jasmine" "Nichole"
  "Rachael" "Chelsea" "Mable" "Ernestine" "Muriel" "Marcella" "Elena"
  "Krystal" "Angelina" "Nadine" "Kari" "Estelle" "Dianna" "Paulette"
  "Lora" "Mona" "Doreen" "Rosemarie" "Angel" "Desiree" "Antonia"
  "Hope" "Ginger" "Janis" "Betsy" "Christie" "Freda" "Mercedes"
  "Meredith" "Lynette" "Teri" "Cristina" "Eula" "Leigh" "Meghan"
  "Sophia" "Eloise" "Rochelle" "Gretchen" "Cecelia" "Raquel"
  "Henrietta" "Alyssa" "Jana" "Kelley" "Gwen" "Kerry" "Jenna" "Tricia"
  "Laverne" "Olive" "Alexis" "Tasha" "Silvia" "Elvira" "Casey" "Delia"
  "Sophie" "Kate" "Patti" "Lorena" "Kellie" "Sonja" "Lila" "Lana"
  "Darla" "May" "Mindy" "Essie" "Mandy" "Lorene" "Elsa" "Josefina"
  "Jeannie" "Miranda" "Dixie" "Lucia" "Marta" "Faith" "Lela" "Johanna"
  "Shari" "Camille" "Tami" "Shawna" "Elisa" "Ebony" "Melba" "Ora"
  "Nettie" "Tabitha" "Ollie" "Jaime" "Winifred" "Kristie" "Marina"
  "Alisha" "Aimee" "Rena" "Myrna" "Marla" "Tammie" "Latasha" "Bonita"
  "Patrice" "Ronda" "Sherrie" "Addie" "Francine" "Deloris" "Stacie"
  "Adriana" "Cheri" "Shelby" "Abigail" "Celeste" "Jewel" "Cara"
  "Adele" "Rebekah" "Lucinda" "Dorthy" "Chris" "Effie" "Trina" "Reba"
  "Shawn" "Sallie" "Aurora" "Lenora" "Etta" "Lottie" "Kerri" "Trisha"
  "Nikki" "Estella" "Francisca" "Josie" "Tracie" "Marissa" "Karin"
  "Brittney" "Janelle" "Lourdes" "Laurel" "Helene" "Fern" "Elva"
  "Corinne" "Kelsey" "Ina" "Bettie" "Elisabeth" "Aida" "Caitlin"
  "Ingrid" "Iva" "Eugenia" "Christa" "Goldie" "Cassie" "Maude"
  "Jenifer" "Therese" "Frankie" "Dena" "Lorna" "Janette" "Latonya"
  "Candy" "Morgan" "Consuelo" "Tamika" "Rosetta" "Debora" "Cherie"
  "Polly" "Dina" "Jewell" "Fay" "Jillian" "Dorothea" "Nell" "Trudy"
  "Esperanza" "Patrica" "Kimberley" "Shanna" "Helena" "Carolina"
  "Cleo" "Stefanie" "Rosario" "Ola" "Janine" "Mollie" "Lupe" "Alisa"
  "Lou" "Maribel" "Susanne" "Bette" "Susana" "Elise" "Cecile"
  "Isabelle" "Lesley" "Jocelyn" "Paige" "Joni" "Rachelle" "Leola"
  "Daphne" "Alta" "Ester" "Petra" "Graciela" "Imogene" "Jolene"
  "Keisha" "Lacey" "Glenna" "Gabriela" "Keri" "Ursula" "Lizzie"
  "Kirsten" "Shana" "Adeline" "Mayra" "Jayne" "Jaclyn" "Gracie"
  "Sondra" "Carmela" "Marisa" "Rosalind" "Charity" "Tonia" "Beatriz"
  "Marisol" "Clarice" "Jeanine" "Sheena" "Angeline" "Frieda" "Lily"
  "Robbie" "Shauna" "Millie" "Claudette" "Cathleen" "Angelia"
  "Gabrielle" "Autumn" "Katharine" "Summer" "Jodie" "Staci" "Lea"
  "Christi" "Jimmie" "Justine" "Elma" "Luella" "Margret" "Dominique"
  "Socorro" "Rene" "Martina" "Margo" "Mavis" "Callie" "Bobbi"
  "Maritza" "Lucile" "Leanne" "Jeannine" "Deana" "Aileen" "Lorie"
  "Ladonna" "Willa" "Manuela" "Gale" "Selma" "Dolly" "Sybil" "Abby"
  "Lara" "Dale" "Ivy" "Dee" "Winnie" "Marcy" "Luisa" "Jeri"
  "Magdalena" "Ofelia" "Meagan" "Audra" "Matilda" "Leila" "Cornelia"
  "Bianca" "Simone" "Bettye" "Randi" "Virgie" "Latisha" "Barbra"
  "Georgina" "Eliza" "Leann" "Bridgette" "Rhoda" "Haley" "Adela"
  "Nola" "Bernadine" "Flossie" "Ila" "Greta" "Ruthie" "Nelda"
  "Minerva" "Lilly" "Terrie" "Letha" "Hilary" "Estela" "Valarie"
  "Brianna" "Rosalyn" "Earline" "Catalina" "Ava" "Mia" "Clarissa"
  "Lidia" "Corrine" "Alexandria" "Concepcion" "Tia" "Sharron" "Rae"
  "Dona" "Ericka" "Jami" "Elnora" "Chandra" "Lenore" "Neva" "Marylou"
  "Melisa" "Tabatha" "Serena" "Avis" "Allie" "Sofia" "Jeanie" "Odessa"
  "Nannie" "Harriett" "Loraine" "Penelope" "Milagros" "Emilia"
  "Benita" "Allyson" "Ashlee" "Tania" "Tommie" "Esmeralda" "Karina"
  "Eve" "Pearlie" "Zelma" "Malinda" "Noreen" "Tameka" "Saundra"
  "Hillary" "Amie" "Althea" "Rosalinda" "Jordan" "Lilia" "Alana" "Gay"
  "Clare" "Alejandra" "Elinor" "Michael" "Lorrie" "Jerri" "Darcy"
  "Earnestine" "Carmella" "Taylor" "Noemi" "Marcie" "Liza" "Annabelle"
  "Louisa" "Earlene" "Mallory" "Carlene" "Nita" "Selena" "Tanisha"
  "Katy" "Julianne" "John" "Lakisha" "Edwina" "Maricela" "Margery"
  "Kenya" "Dollie" "Roxie" "Roslyn" "Kathrine" "Nanette" "Charmaine"
  "Lavonne" "Ilene" "Kris" "Tammi" "Suzette" "Corine" "Kaye" "Jerry"
  "Merle" "Chrystal" "Lina" "Deanne" "Lilian" "Juliana" "Aline"
  "Luann" "Kasey" "Maryanne" "Evangeline" "Colette" "Melva" "Lawanda"
  "Yesenia" "Nadia" "Madge" "Kathie" "Eddie" "Ophelia" "Valeria"
  "Nona" "Mitzi" "Mari" "Georgette" "Claudine" "Fran" "Alissa"
  "Roseann" "Lakeisha" "Susanna" "Reva" "Deidre" "Chasity" "Sheree"
  "Carly" "James" "Elvia" "Alyce" "Deirdre" "Gena" "Briana" "Araceli"
  "Katelyn" "Rosanne" "Wendi" "Tessa" "Berta" "Marva" "Imelda"
  "Marietta" "Marci" "Leonor" "Arline" "Sasha" "Madelyn" "Janna"
  "Juliette" "Deena" "Aurelia" "Josefa" "Augusta" "Liliana" "Young"
  "Christian" "Lessie" "Amalia" "Savannah" "Anastasia" "Vilma"
  "Natalia" "Rosella" "Lynnette" "Corina" "Alfreda" "Leanna" "Carey"
  "Amparo" "Coleen" "Tamra" "Aisha" "Wilda" "Karyn" "Cherry" "Queen"
  "Maura" "Mai" "Evangelina" "Rosanna" "Hallie" "Erna" "Enid"
  "Mariana" "Lacy" "Juliet" "Jacklyn" "Freida" "Madeleine" "Mara"
  "Hester" "Cathryn" "Lelia" "Casandra" "Bridgett" "Angelita" "Jannie"
  "Dionne" "Annmarie" "Katina" "Beryl" "Phoebe" "Millicent" "Katheryn"
  "Diann" "Carissa" "Maryellen" "Liz" "Lauri" "Helga" "Gilda" "Adrian"
  "Rhea" "Marquita" "Hollie" "Tisha" "Tamera" "Angelique" "Francesca"
  "Britney" "Kaitlin" "Lolita" "Florine" "Rowena" "Reyna" "Twila"
  "Fanny" "Janell" "Ines" "Concetta" "Bertie" "Alba" "Brigitte"
  "Alyson" "Vonda" "Pansy" "Elba" "Noelle" "Letitia" "Kitty" "Deann"
  "Brandie" "Louella" "Leta" "Felecia" "Sharlene" "Lesa" "Beverley"
  "Robert" "Isabella" "Herminia" "Terra" "Celina"))

(defvar test-data-last-names
  '("Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller"
  "Wilson" "Moore" "Taylor" "Anderson" "Thomas" "Jackson" "White"
  "Harris" "Martin" "Thompson" "Garcia" "Martinez" "Robinson" "Clark"
  "Rodriguez" "Lewis" "Lee" "Walker" "Hall" "Allen" "Young"
  "Hernandez" "King" "Wright" "Lopez" "Hill" "Scott" "Green" "Adams"
  "Baker" "Gonzalez" "Nelson" "Carter" "Mitchell" "Perez" "Roberts"
  "Turner" "Phillips" "Campbell" "Parker" "Evans" "Edwards" "Collins"
  "Stewart" "Sanchez" "Morris" "Rogers" "Reed" "Cook" "Morgan" "Bell"
  "Murphy" "Bailey" "Rivera" "Cooper" "Richardson" "Cox" "Howard"
  "Ward" "Torres" "Peterson" "Gray" "Ramirez" "James" "Watson"
  "Brooks" "Kelly" "Sanders" "Price" "Bennett" "Wood" "Barnes" "Ross"
  "Henderson" "Coleman" "Jenkins" "Perry" "Powell" "Long" "Patterson"
  "Hughes" "Flores" "Washington" "Butler" "Simmons" "Foster"
  "Gonzales" "Bryant" "Alexander" "Russell" "Griffin" "Diaz" "Hayes"
  "Myers" "Ford" "Hamilton" "Graham" "Sullivan" "Wallace" "Woods"
  "Cole" "West" "Jordan" "Owens" "Reynolds" "Fisher" "Ellis"
  "Harrison" "Gibson" "Mcdonald" "Cruz" "Marshall" "Ortiz" "Gomez"
  "Murray" "Freeman" "Wells" "Webb" "Simpson" "Stevens" "Tucker"
  "Porter" "Hunter" "Hicks" "Crawford" "Henry" "Boyd" "Mason"
  "Morales" "Kennedy" "Warren" "Dixon" "Ramos" "Reyes" "Burns"
  "Gordon" "Shaw" "Holmes" "Rice" "Robertson" "Hunt" "Black" "Daniels"
  "Palmer" "Mills" "Nichols" "Grant" "Knight" "Ferguson" "Rose"
  "Stone" "Hawkins" "Dunn" "Perkins" "Hudson" "Spencer" "Gardner"
  "Stephens" "Payne" "Pierce" "Berry" "Matthews" "Arnold" "Wagner"
  "Willis" "Ray" "Watkins" "Olson" "Carroll" "Duncan" "Snyder" "Hart"
  "Cunningham" "Bradley" "Lane" "Andrews" "Ruiz" "Harper" "Fox"
  "Riley" "Armstrong" "Carpenter" "Weaver" "Greene" "Lawrence"
  "Elliott" "Chavez" "Sims" "Austin" "Peters" "Kelley" "Franklin"
  "Lawson" "Fields" "Gutierrez" "Ryan" "Schmidt" "Carr" "Vasquez"
  "Castillo" "Wheeler" "Chapman" "Oliver" "Montgomery" "Richards"
  "Williamson" "Johnston" "Banks" "Meyer" "Bishop" "Mccoy" "Howell"
  "Alvarez" "Morrison" "Hansen" "Fernandez" "Garza" "Harvey" "Little"
  "Burton" "Stanley" "Nguyen" "George" "Jacobs" "Reid" "Kim" "Fuller"
  "Lynch" "Dean" "Gilbert" "Garrett" "Romero" "Welch" "Larson"
  "Frazier" "Burke" "Hanson" "Day" "Mendoza" "Moreno" "Bowman"
  "Medina" "Fowler" "Brewer" "Hoffman" "Carlson" "Silva" "Pearson"
  "Holland" "Douglas" "Fleming" "Jensen" "Vargas" "Byrd" "Davidson"
  "Hopkins" "May" "Terry" "Herrera" "Wade" "Soto" "Walters" "Curtis"
  "Neal" "Caldwell" "Lowe" "Jennings" "Barnett" "Graves" "Jimenez"
  "Horton" "Shelton" "Barrett" "Obrien" "Castro" "Sutton" "Gregory"
  "Mckinney" "Lucas" "Miles" "Craig" "Rodriquez" "Chambers" "Holt"
  "Lambert" "Fletcher" "Watts" "Bates" "Hale" "Rhodes" "Pena" "Beck"
  "Newman" "Haynes" "Mcdaniel" "Mendez" "Bush" "Vaughn" "Parks"
  "Dawson" "Santiago" "Norris" "Hardy" "Love" "Steele" "Curry"
  "Powers" "Schultz" "Barker" "Guzman" "Page" "Munoz" "Ball" "Keller"
  "Chandler" "Weber" "Leonard" "Walsh" "Lyons" "Ramsey" "Wolfe"
  "Schneider" "Mullins" "Benson" "Sharp" "Bowen" "Daniel" "Barber"
  "Cummings" "Hines" "Baldwin" "Griffith" "Valdez" "Hubbard" "Salazar"
  "Reeves" "Warner" "Stevenson" "Burgess" "Santos" "Tate" "Cross"
  "Garner" "Mann" "Mack" "Moss" "Thornton" "Dennis" "Mcgee" "Farmer"
  "Delgado" "Aguilar" "Vega" "Glover" "Manning" "Cohen" "Harmon"
  "Rodgers" "Robbins" "Newton" "Todd" "Blair" "Higgins" "Ingram"
  "Reese" "Cannon" "Strickland" "Townsend" "Potter" "Goodwin" "Walton"
  "Rowe" "Hampton" "Ortega" "Patton" "Swanson" "Joseph" "Francis"
  "Goodman" "Maldonado" "Yates" "Becker" "Erickson" "Hodges" "Rios"
  "Conner" "Adkins" "Webster" "Norman" "Malone" "Hammond" "Flowers"
  "Cobb" "Moody" "Quinn" "Blake" "Maxwell" "Pope" "Floyd" "Osborne"
  "Paul" "Mccarthy" "Guerrero" "Lindsey" "Estrada" "Sandoval" "Gibbs"
  "Tyler" "Gross" "Fitzgerald" "Stokes" "Doyle" "Sherman" "Saunders"
  "Wise" "Colon" "Gill" "Alvarado" "Greer" "Padilla" "Simon" "Waters"
  "Nunez" "Ballard" "Schwartz" "Mcbride" "Houston" "Christensen"
  "Klein" "Pratt" "Briggs" "Parsons" "Mclaughlin" "Zimmerman" "French"
  "Buchanan" "Moran" "Copeland" "Roy" "Pittman" "Brady" "Mccormick"
  "Holloway" "Brock" "Poole" "Frank" "Logan" "Owen" "Bass" "Marsh"
  "Drake" "Wong" "Jefferson" "Park" "Morton" "Abbott" "Sparks"
  "Patrick" "Norton" "Huff" "Clayton" "Massey" "Lloyd" "Figueroa"
  "Carson" "Bowers" "Roberson" "Barton" "Tran" "Lamb" "Harrington"
  "Casey" "Boone" "Cortez" "Clarke" "Mathis" "Singleton" "Wilkins"
  "Cain" "Bryan" "Underwood" "Hogan" "Mckenzie" "Collier" "Luna"
  "Phelps" "Mcguire" "Allison" "Bridges" "Wilkerson" "Nash" "Summers"
  "Atkins" "Wilcox" "Pitts" "Conley" "Marquez" "Burnett" "Richard"
  "Cochran" "Chase" "Davenport" "Hood" "Gates" "Clay" "Ayala" "Sawyer"
  "Roman" "Vazquez" "Dickerson" "Hodge" "Acosta" "Flynn" "Espinoza"
  "Nicholson" "Monroe" "Wolf" "Morrow" "Kirk" "Randall" "Anthony"
  "Whitaker" "Oconnor" "Skinner" "Ware" "Molina" "Kirby" "Huffman"
  "Bradford" "Charles" "Gilmore" "Dominguez" "Oneal" "Bruce" "Lang"
  "Combs" "Kramer" "Heath" "Hancock" "Gallagher" "Gaines" "Shaffer"
  "Short" "Wiggins" "Mathews" "Mcclain" "Fischer" "Wall" "Small"
  "Melton" "Hensley" "Bond" "Dyer" "Cameron" "Grimes" "Contreras"
  "Christian" "Wyatt" "Baxter" "Snow" "Mosley" "Shepherd" "Larsen"
  "Hoover" "Beasley" "Glenn" "Petersen" "Whitehead" "Meyers" "Keith"
  "Garrison" "Vincent" "Shields" "Horn" "Savage" "Olsen" "Schroeder"
  "Hartman" "Woodard" "Mueller" "Kemp" "Deleon" "Booth" "Patel"
  "Calhoun" "Wiley" "Eaton" "Cline" "Navarro" "Harrell" "Lester"
  "Humphrey" "Parrish" "Duran" "Hutchinson" "Hess" "Dorsey" "Bullock"
  "Robles" "Beard" "Dalton" "Avila" "Vance" "Rich" "Blackwell" "York"
  "Johns" "Blankenship" "Trevino" "Salinas" "Campos" "Pruitt" "Moses"
  "Callahan" "Golden" "Montoya" "Hardin" "Guerra" "Mcdowell" "Carey"
  "Stafford" "Gallegos" "Henson" "Wilkinson" "Booker" "Merritt"
  "Miranda" "Atkinson" "Orr" "Decker" "Hobbs" "Preston" "Tanner"
  "Knox" "Pacheco" "Stephenson" "Glass" "Rojas" "Serrano" "Marks"
  "Hickman" "English" "Sweeney" "Strong" "Prince" "Mcclure" "Conway"
  "Walter" "Roth" "Maynard" "Farrell" "Lowery" "Hurst" "Nixon" "Weiss"
  "Trujillo" "Ellison" "Sloan" "Juarez" "Winters" "Mclean" "Randolph"
  "Leon" "Boyer" "Villarreal" "Mccall" "Gentry" "Carrillo" "Kent"
  "Ayers" "Lara" "Shannon" "Sexton" "Pace" "Hull" "Leblanc" "Browning"
  "Velasquez" "Leach" "Chang" "House" "Sellers" "Herring" "Noble"
  "Foley" "Bartlett" "Mercado" "Landry" "Durham" "Walls" "Barr"
  "Mckee" "Bauer" "Rivers" "Everett" "Bradshaw" "Pugh" "Velez" "Rush"
  "Estes" "Dodson" "Morse" "Sheppard" "Weeks" "Camacho" "Bean"
  "Barron" "Livingston" "Middleton" "Spears" "Branch" "Blevins" "Chen"
  "Kerr" "Mcconnell" "Hatfield" "Harding" "Ashley" "Solis" "Herman"
  "Frost" "Giles" "Blackburn" "William" "Pennington" "Woodward"
  "Finley" "Mcintosh" "Koch" "Best" "Solomon" "Mccullough" "Dudley"
  "Nolan" "Blanchard" "Rivas" "Brennan" "Mejia" "Kane" "Benton"
  "Joyce" "Buckley" "Haley" "Valentine" "Maddox" "Russo" "Mcknight"
  "Buck" "Moon" "Mcmillan" "Crosby" "Berg" "Dotson" "Mays" "Roach"
  "Church" "Chan" "Richmond" "Meadows" "Faulkner" "Oneill" "Knapp"
  "Kline" "Barry" "Ochoa" "Jacobson" "Gay" "Avery" "Hendricks" "Horne"
  "Shepard" "Hebert" "Cherry" "Cardenas" "Mcintyre" "Whitney" "Waller"
  "Holman" "Donaldson" "Cantu" "Terrell" "Morin" "Gillespie" "Fuentes"
  "Tillman" "Sanford" "Bentley" "Peck" "Key" "Salas" "Rollins"
  "Gamble" "Dickson" "Battle" "Santana" "Cabrera" "Cervantes" "Howe"
  "Hinton" "Hurley" "Spence" "Zamora" "Yang" "Mcneil" "Suarez" "Case"
  "Petty" "Gould" "Mcfarland" "Sampson" "Carver" "Bray" "Rosario"
  "Macdonald" "Stout" "Hester" "Melendez" "Dillon" "Farley" "Hopper"
  "Galloway" "Potts" "Bernard" "Joyner" "Stein" "Aguirre" "Osborn"
  "Mercer" "Bender" "Franco" "Rowland" "Sykes" "Benjamin" "Travis"
  "Pickett" "Crane" "Sears" "Mayo" "Dunlap" "Hayden" "Wilder" "Mckay"
  "Coffey" "Mccarty" "Ewing" "Cooley" "Vaughan" "Bonner" "Cotton"
  "Holder" "Stark" "Ferrell" "Cantrell" "Fulton" "Lynn" "Lott"
  "Calderon" "Rosa" "Pollard" "Hooper" "Burch" "Mullen" "Fry" "Riddle"
  "Levy" "David" "Duke" "Odonnell" "Guy" "Michael" "Britt" "Frederick"
  "Daugherty" "Berger" "Dillard" "Alston" "Jarvis" "Frye" "Riggs"
  "Chaney" "Odom" "Duffy" "Fitzpatrick" "Valenzuela" "Merrill" "Mayer"
  "Alford" "Mcpherson" "Acevedo" "Donovan" "Barrera" "Albert" "Cote"
  "Reilly" "Compton" "Raymond" "Mooney" "Mcgowan" "Craft" "Cleveland"
  "Clemons" "Wynn" "Nielsen" "Baird" "Stanton" "Snider" "Rosales"
  "Bright" "Witt" "Stuart" "Hays" "Holden" "Rutledge" "Kinney"
  "Clements" "Castaneda" "Slater" "Hahn" "Emerson" "Conrad" "Burks"
  "Delaney" "Pate" "Lancaster" "Sweet" "Justice" "Tyson" "Sharpe"
  "Whitfield" "Talley" "Macias" "Irwin" "Burris" "Ratliff" "Mccray"
  "Madden" "Kaufman" "Beach" "Goff" "Cash" "Bolton" "Mcfadden"
  "Levine" "Good" "Byers" "Kirkland" "Kidd" "Workman" "Carney" "Dale"
  "Mcleod" "Holcomb" "England" "Finch" "Head" "Burt" "Hendrix" "Sosa"
  "Haney" "Franks" "Sargent" "Nieves" "Downs" "Rasmussen" "Bird"
  "Hewitt" "Lindsay" "Le" "Foreman" "Valencia" "Oneil" "Delacruz"
  "Vinson" "Dejesus" "Hyde" "Forbes" "Gilliam" "Guthrie" "Wooten"
  "Huber" "Barlow" "Boyle" "Mcmahon" "Buckner" "Rocha" "Puckett"
  "Langley" "Knowles" "Cooke" "Velazquez" "Whitley" "Noel" "Vang"))

(defvar test-data-salutations
  '((male "Dr" "Mr")
    (female "Dr" "Ms" "Mrs")))


(defvar test-data-company-names
  '("Acme, inc." "Widget Corp" "123 Warehousing" "Demo Company"
    "Smith and Co." "Foo Bars" "ABC Telecom" "Fake Brothers"
    "QWERTY Logistics" "Demo, inc." "Sample Company" "Sample, inc"
    "Acme Corp" "Allied Biscuit" "Ankh-Sto Associates"
    "Extensive Enterprise" "Galaxy Corp" "Globo-Chem" "Mr. Sparkle"
    "Globex Corporation" "LexCorp" "LuthorCorp"
    "North Central Positronics" "Omni Consimer Products"
    "Praxis Corporation" "Sombra Corporation" "Sto Plains Holdings"
    "Tessier-Ashpool" "Wayne Enterprises" "Wentworth Industries"
    "ZiffCorp" "Bluth Company" "Strickland Propane" "Thatherton Fuels"
    "Three Waters" "Water and Power" "Western Gas & Electric"
    "Mammoth Pictures" "Mooby Corp" "Gringotts" "Thrift Bank"
    "Flowers By Irene" "The Legitimate Businessmens Club"
    "Osato Chemicals" "Transworld Consortium" "Universal Export"
    "United Fried Chicken" "Virtucon" "Kumatsu Motors"
    "Keedsler Motors" "Powell Motors" "Industrial Automation"
    "Sirius Cybernetics Corporation" "U.S. Robotics and Mechanical Men"
    "Colonial Movers" "Corellian Engineering Corporation"
    "Incom Corporation" "General Products" "Leeding Engines Ltd."
    "Blammo" "Input, Inc." "Mainway Toys" "Videlectrix" "Zevo Toys"
    "Ajax" "Axis Chemical Co." "Barrytron" "Carrys Candles"
    "Cogswell Cogs" "Spacely Sprockets" "General Forge and Foundry"
    "Duff Brewing Company" "Dunder Mifflin"
    "General Services Corporation" "Monarch Playing Card Co."
    "Krustyco" "Initech" "Roboto Industries" "Primatech"
    "Sonky Rubber Goods" "St. Anky Beer" "Stay Puft Corporation"
    "Vandelay Industries" "Wernham Hogg" "Gadgetron"
    "Burleigh and Stronginthearm" "BLAND Corporation"
    "Nordyne Defense Dynamics" "Petrox Oil Company" "Roxxon"
    "McMahon and Tate" "Sixty Second Avenue" "Charles Townsend Agency"
    "Spade and Archer" "Megadodo Publications" "Rouster and Sideways"
    "C.H. Lavatory and Sons" "Globo Gym American Corp" "The New Firm"
    "SpringShield" "Compuglobalhypermeganet" "Data Systems"
    "Gizmonic Institute" "Initrode" "Taggart Transcontinental"
    "Atlantic Northern" "Niagular" "Plow King" "Big Kahuna Burger"
    "Big T Burgers and Fries" "Chez Quis" "Chotchkies"
    "The Frying Dutchman" "Klimpys" "The Krusty Krab" "Monks Diner"
    "Milliways" "Minuteman Cafe" "Taco Grande" "Tip Top Cafe"
    "Moes Tavern" "Central Perk" "Chasers"))

(defvar test-data-street-names
  '("Second" "Third" "First" "Fourth" "Park" "Fifth" "Main" "Sixth"
  "Oak" "Seventh" "Pine" "Maple" "Cedar" "Eighth" "Elm" "View"
  "Washington" "Ninth" "Lake" "Hill" "Birch" "Railway" "River"
  "Spruce" "Mill" "Church" "Poplar" "Willow" "Victoria" "King"
  "Sunset" "Lake" "Aspen" "Centre" "Mountain" "Lakeview" "Queen"
  "Tenth" "Eleventh" "James" "Smith" "George" "Martin" "Albert"
  "Campbell" "William" "North" "Woodland" "Wilson" "Elizabeth"
  "Riverside" "Bellevue" "Beach" "John" "Hillcrest" "Bayview" "Scott"
  "Evergreen" "Charles" "Ross" "Forest" "Taylor" "Nelson" "Roy"
  "Wellington" "Fir" "Hillside" "Riverview" "Thompson" "Pioneer"
  "Fiftieth" "Fraser" "Water" "Thomas" "West" "Hill" "Cameron"
  "Centennial" "Forty Ninth" "Douglas" "Robert"))

(defvar test-data-street-suffixes
  '("St" "Ave" "Rd"))

(defvar test-data-street-directions
  '("E" "N" "NE" "NW" "S" "SE" "SW" "W"))

(defvar test-data-street-units
  '("Apt" "Unit"))

;; province, province short code, population, towns
(defvar test-data-towns-canada
  '(("Alberta" "AB" 3725 ("Airdrie" "Brooks" "Calgary" "Camrose"
                     "Cold Lake" "Edmonton" "Fort Saskatchewan"
  "Grande Prairie" "Lacombe" "Leduc" "Lethbridge" "Lloydminster"
  "Medicine Hat" "Red Deer" "Spruce Grove" "St. Albert" "Wetaskiwin"))
    ("British Columbia" "BC" 4511 ("Abbotsford" "Armstrong" "Burnaby"
                         "Campbell River" "Castlegar" "Chilliwack"
  "Colwood" "Coquitlam" "Courtenay" "Cranbrook" "Dawson Creek"
  "Duncan" "Enderby" "Fernie" "Fort St. John" "Grand Forks"
  "Greenwood" "Kamloops" "Kelowna" "Kimberley" "Kitimat" "Langford"
  "Langley" "Merritt" "Nanaimo" "Nelson" "New Westminster" "North
  Vancouver" "Parksville" "Penticton" "Pitt Meadows" "Port Alberni"
  "Port Coquitlam" "Port Moody" "Powell River" "Prince George" "Prince
  Rupert" "Quesnel" "Revelstoke" "Richmond" "Rossland" "Salmon Arm"
  "Surrey" "Terrace" "Trail" "Vancouver" "Vernon" "Victoria" "White
  Rock" "Williams Lake"))
    ("Manitoba" "MB" 1233 ("Brandon" "Dauphin" "Flin Flon" "Portage la
  Prairie" "Selkirk" "Steinbach" "Thompson" "Winkler" "Winnipeg"))
    ("New Brunswick" "NB" 751 ("Bathurst" "Campbellton" "Dieppe"
  "Edmundston" "Fredericton" "Miramichi" "Moncton" "Saint John"))
    ("Newfoundland and Labrador" "NL" 511 ("Corner Brook" "Mount Pearl"
                                       "St. John's"))
    ("Northwest Territories" "NT" 43 ("Yellowknife"))
    ("Nova Scotia" "NS"  940 ("Halifax" "Sydney" "Dartmouth"))
    ("Nunavut" "NU" 32 ("Iqaluit"))
    ("Ontario" "ON" 13167 ("Barrie" "Belleville" "Brampton" "Brant"
  "Brantford" "Brockville" "Burlington" "Cambridge" "Chatham-Kent"
  "Clarence-Rockland" "Cornwall" "Dryden" "Elliot Lake" "Greater
  Sudbury" "Guelph" "Haldimand County" "Hamilton" "Kawartha Lakes"
  "Kenora" "Kingston" "Kitchener" "London" "Mississauga" "Niagara
  Falls" "Norfolk County" "North Bay" "Orillia" "Oshawa" "Ottawa"
  "Owen Sound" "Pembroke" "Peterborough" "Pickering" "Prince Edward
  County" "Port Colborne" "Quinte West" "Sarnia" "Sault Ste. Marie"
  "St. Catharines" "St. Thomas" "Stratford" "Temiskaming Shores"
  "Thorold" "Thunder Bay" "Timmins" "Toronto" "Vaughan" "Waterloo"
  "Welland" "Windsor" "Woodstock"))
    ("Prince Edward Island" "PE" 142 ("Charlottetown" "Summerside"))
    ("Quebec" "QC" 7886 ("Acton Vale" "Alma" "Amos" "Amqui"
  "L'Ancienne-Lorette" "Asbestos" "L'Assomption" "Baie-Comeau"
  "Baie-d'Urfé" "Baie-Saint-Paul" "Barkmere" "Beaconsfield"
  "Beauceville" "Beauharnois" "Beaupré" "Bécancour" "Bedford"
  "Belleterre" "Belœil" "Berthierville" "Blainville" "Bois-des-Filion"
  "Boisbriand" "Bonaventure" "Boucherville" "Brome Lake (Lac-Brome)"
  "Bromont" "Brossard" "Brownsburg-Chatham" "Cabano" "Candiac"
  "Cap-Chat" "Cap-Santé" "Carignan" "Carleton-sur-Mer" "Causapscal"
  "Chambly" "Chandler" "Chapais" "Charlemagne" "Châteauguay"
  "Château-Richer" "Chibougamau" "Clermont" "Coaticook" "Contrecoeur"
  "Cookshire-Eaton" "Côte-Saint-Luc" "Cowansville" "Danville"
  "Daveluyville" "Dégelis" "Delson" "Desbiens" "Deux-Montagnes"
  "Disraeli" "Dolbeau-Mistassini" "Dollard-des-Ormeaux" "Donnacona"
  "Dorval" "Drummondville" "Dunham" "Duparquet" "East Angus"
  "L'Épiphanie" "Estérel" "Farnham" "Fermont" "Forestville"
  "Fossambault-sur-le-Lac" "Gaspé" "Gatineau" "Gracefield" "Granby"
  "Grande-Rivière" "Hampstead" "Hudson" "Huntingdon" "L'Île-Cadieux"
  "L'Île-Dorval" "L'Île-Perrot" "Joliette" "Kingsey Falls" "Kirkland"
  "Lac-Delage" "Lac-Mégantic" "Lac-Saint-Joseph" "Lac-Sergent"
  "Lachute" "Laval" "Lavaltrie" "Lebel-sur-Quévillon" "Léry" "Lévis"
  "Longueuil" "Lorraine" "Louiseville" "Macamic" "Magog" "Malartic"
  "La Malbaie" "Maniwaki" "Marieville" "Mascouche" "Matagami" "Matane"
  "Mercier" "Métabetchouan–Lac-à-la-Croix" "Métis-sur-Mer" "Mirabel"
  "Mont-Joli" "Mont-Laurier" "Mont-Saint-Hilaire" "Mont-Tremblant"
  "Montmagny" "Montreal" "Montréal-Est" "Montreal West" "Mount Royal"
  "Murdochville" "Neuville" "New Richmond" "Nicolet" "Normandin"
  "Notre-Dame-de-l'Île-Perrot" "Notre-Dame-des-Prairies"
  "Notre-Dame-du-Lac" "Otterburn Park" "Paspébiac" "Percé" "Pincourt"
  "Plessisville" "La Pocatière" "Pohénégamook" "Pointe-Claire"
  "Pont-Rouge" "Port-Cartier" "Portneuf" "La Prairie" "Princeville"
  "Prévost" "Quebec City" "Repentigny" "Richelieu" "Richmond"
  "Rimouski" "Rivière-du-Loup" "Rivière-Rouge" "Roberval" "Rosemère"
  "Rouyn-Noranda" "Saguenay" "Sainte-Adèle" "Sainte-Agathe-des-Monts"
  "Sainte-Anne-de-Beaupré" "Sainte-Anne-de-Bellevue"
  "Sainte-Anne-des-Monts" "Sainte-Anne-des-Plaines"
  "Saint-Augustin-de-Desmaures" "Saint-Basile" "Saint-Basile-le-Grand"
  "Saint-Bruno-de-Montarville" "Sainte-Catherine"
  "Sainte-Catherine-de-la-Jacques-Cartier" "Saint-Césaire"
  "Saint-Constant" "Saint-Eustache" "Saint-Félicien" "Saint-Gabriel"
  "Saint-Georges" "Saint-Hyacinthe" "Saint-Jean-sur-Richelieu"
  "Saint-Jérôme" "Saint-Joseph-de-Beauce" "Saint-Joseph-de-Sorel"
  "Sainte-Julie" "Saint-Lambert" "Saint-Lazare"
  "Saint-Lin-Laurentides" "Saint-Marc-des-Carrières"
  "Sainte-Marguerite-du-Lac-Masson" "Sainte-Marie"
  "Sainte-Marthe-sur-le-Lac" "Saint-Ours" "Saint-Pamphile"
  "Saint-Pascal" "Saint-Pie" "Saint-Raymond" "Saint-Rémi"
  "Saint-Sauveur" "Sainte-Thérèse" "Saint-Tite"
  "Salaberry-de-Valleyfield" "La Sarre" "Schefferville" "Scotstown"
  "Senneterre" "Sept-Îles" "Shawinigan" "Sherbrooke" "Sorel-Tracy"
  "Stanstead" "Sutton" "Témiscaming" "Terrebonne" "Thetford Mines"
  "Thurso" "Trois-Pistoles" "Trois-Rivières" "La Tuque" "Val-d'Or"
  "Valcourt" "Varennes" "Vaudreuil-Dorion" "Victoriaville"
  "Ville-Marie" "Warwick" "Waterloo" "Waterville" "Westmount"
  "Windsor"))
    ("Saskatchewan" "SK" 1042 ("Estevan" "Flin Flon" "Humboldt"
  "Lloydminster" "Martensville" "Meadow Lake" "Melfort" "Melville"
  "Moose Jaw" "North Battleford" "Prince Albert" "Regina" "Saskatoon"
  "Swift Current" "Weyburn" "Yorkton"))
    ("Yukon" "YT" 34 ("Whitehorse"))
    ))

(defvar test-data-postal-codes
  '(("NL" "A")
    ("NS" "B")
    ("PE" "C")
    ("NB" "E")
    ("QC" "G" "H" "J")
    ("ON" "K" "L" "M" "N" "P")
    ("MB" "R")
    ("SK" "S")
    ("AB" "T")
    ("BC" "V")
    ("NU" "X")
    ("NT" "X")
    ("YT" "Y")
    ))

(defvar test-data-phone-areacodes
  '(("AB" "403" "587" "780")
    ("BC" "236" "250" "604" "778")
    ("MB" "204" "431")
    ("NB" "506")
    ("NL" "709")
    ("NT" "867")
    ("NS" "902")
    ("NU" "867")
    ("ON" "226" "249" "289" "343" "365" "416" "437" "519" "613" "647"
     "705" "807" "905")
    ("QC" "418" "438" "450" "514" "579" "581" "819" "873")
    ("SK" "306" "639")
    ("YT" "867")
    ))

(defvar test-data-weights
  '((streetdirection . 50)
    (streetunit . 10)
    (box . 20)
    (company . 5)))

(defun test-data-pick-gender ()
  (test-data-random-element '(male female)))

(defun test-data-pick-first-name (&optional gender)
  (if (eq gender nil)
      (setq gender (test-data-pick-gender)))
  (if (eq gender 'male)
      (test-data-random-element test-data-first-names-male)
    (test-data-random-element test-data-first-names-female)))

(defun test-data-pick-last-name ()
  (test-data-random-element test-data-last-names))

(defun test-data-pick-name (&optional gender)
  (concat (test-data-pick-first-name gender) " " (test-data-pick-last-name)))

(defun test-data-pick-company-name ()
  (test-data-random-element test-data-company-names))

(defun test-data-pick-salutation (&optional gender)
  (test-data-random-element
   (cdr (assoc (if (eq gender nil)
                   (test-data-pick-gender) gender)
               test-data-salutations))))

(defun test-data-pick-housenumber ()
  (number-to-string (random 500)))

(defun test-data-pick-street ()
  (test-data-random-element test-data-street-names))

(defun test-data-pick-streetsuffix ()
  (test-data-random-element test-data-street-suffixes))

(defun test-data-pick-streetdirection ()
  (test-data-random-element test-data-street-directions))

(defun test-data-pick-streetunit ()
  (test-data-random-element test-data-street-units))

(defun test-data-pick-streetunitnumber ()
  (number-to-string (random 5)))

(defun test-data-pick-postal-code (&optional provincecode)
  (concat
   (test-data-random-element
    (cdr
     (if (eq provincecode nil)
         (test-data-random-element test-data-postal-codes)
       (assoc provincecode test-data-postal-codes))))
   (number-to-string (random 9))
   (make-string 1 (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 25)))
   " "
   (number-to-string (random 9))
   (make-string 1 (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 25)))
   (number-to-string (random 9))
   ))

(defun test-data-pick-pobox ()
  (number-to-string (+ 50 (random 100))))
  
(defun test-data-pick-province ()
  (let ((total 0)
        index)

    ;; figure out total population of all provinces combined
    (dolist (province test-data-towns-canada)
      (setq total (+ total (nth 2 province))))

    ;; pick a random number below the total
    (setq index (random total))
    
    ;; substract province populations until 0 is reached
    ;; return that province
    (let ((provinces test-data-towns-canada))
      (while (> index 0) 
        (setq index (- index (nth 2 (car provinces))))
        (if (> index 0)
            (setq provinces (cdr provinces))))
      (car provinces))))

(defun test-data-pick-town (&optional province)
  (if (eq province nil)
      (setq province (test-data-pick-province)))
  (test-data-random-element (nth 3 province)))

(defun test-data-pick-personemail (firstname lastname)
  (concat
   (substring (downcase firstname) 0 1)
   "."
   (downcase lastname)
   "@example.com"))

(defun test-data-pick-companyemail (company)
  (concat
   (replace-regexp-in-string "[^a-z-]" "" (replace-regexp-in-string " +" "-" (downcase company)))
   "@example.com"))

(defun test-data-pick-phone (&optional provincecode)
  (concat
   (test-data-random-element
    (cdr
     (if (eq provincecode nil)
         (test-data-random-element test-data-phone-areacodes)
       (assoc provincecode test-data-phone-areacodes))))
   "-"
   (number-to-string (+ 200 (random 799)))
   "-"
   (number-to-string (+ 1000 (random 8999)))))



(defun test-data-pick-address (address weights)
  (let ((province))

    (if (< (random 100) (cdr (assoc 'box weights)))
        (setcdr (assoc 'box address) (test-data-pick-pobox))
      (progn
        (setcdr (assoc 'housenumber address) (test-data-pick-housenumber))
        (setcdr (assoc 'street address) (test-data-pick-street))
        (setcdr (assoc 'streetsuffix address) (test-data-pick-streetsuffix))
        (if (< (random 100) (cdr (assoc 'streetdirection weights)))
            (setcdr (assoc 'streetdirection address) (test-data-pick-streetdirection)))
        (if (< (random 100) (cdr (assoc 'streetunit weights)))
            (progn
              (setcdr (assoc 'unit address) (test-data-pick-streetunit))
              (setcdr (assoc 'unitnumber address)
                      (test-data-pick-streetunitnumber))))))

    (if (cdr (assoc 'province address))
        (setq province (assoc (cdr (assoc 'province address)) test-data-towns-canada))
      (progn
        (setq province (test-data-pick-province))
        (setcdr (assoc 'province address) (car province))))

    (if (not (cdr (assoc 'town address)))
        (setcdr (assoc 'town address) (test-data-random-element (nth 3 province))))
    (if (not (cdr (assoc 'postalcode address)))
        (setcdr (assoc 'postalcode address) (test-data-pick-postal-code
                                             (nth 1 province))))

    
    address))

(defun test-data-pick-profile (profile weights)
  (if (< (random 100) (cdr (assoc 'company weights)))
      (progn
        (setcdr (assoc 'company profile) (test-data-pick-company-name))
        (setcdr (assoc 'email profile)
                (test-data-pick-companyemail
                 (cdr (assoc 'company profile)))))
    (progn
      (setcdr (assoc 'gender profile) (test-data-pick-gender))
      (setcdr (assoc 'salutation profile)
              (test-data-pick-salutation (cdr (assoc 'gender profile))))
      (setcdr (assoc 'firstname profile)
              (test-data-pick-first-name (cdr (assoc 'gender profile))))
      (setcdr (assoc 'lastname profile) (test-data-pick-last-name))
      (setcdr (assoc 'email profile)
              (test-data-pick-personemail
               (cdr (assoc 'firstname profile))
               (cdr (assoc 'lastname profile))))))
  (test-data-pick-address (cdr (assoc 'address profile)) weights)
  (setcdr (assoc 'phone profile)
          (test-data-pick-phone
           (nth 1 (cdr (assoc 'province (cdr (assoc 'address profile)))))))
  profile)


(defun test-data-init-address ()
  '((housenumber . nil)
        (street . nil)
        (streetsuffix . nil)
        (streetdirection . nil)
        (province . nil)
        (town . nil)
        (postalcode . nil)
        (box . nil)
        (unit . nil)
        (unitnumber . nil)))

(defun test-data-init-profile ()
  (list '(gender . nil)
        '(salutation . nil)
        '(firstname . nil)
        '(lastname . nil)
        '(company . nil)
        (cons 'address (test-data-init-address))
        '(email . nil)
        '(phone . nil)))


(defun test-data-random-element (list)
  (nth (random (safe-length list)) list))

(defun test-data-insert-address (address-buffer address)
  (let ((address-i))
    (with-current-buffer address-buffer
      (setq address-i (number-to-string (line-number-at-pos)))
      (insert
       address-i ","
       (or (cdr (assoc 'housenumber address)) "") ","
       (or (cdr (assoc 'street address)) "") ","
       (or (cdr (assoc 'streetsuffix address)) "") ","
       (or (cdr (assoc 'streetdirection address)) "") ","
       (or (cdr (assoc 'box address)) "") ","
       (or (cdr (assoc 'unit address)) "") ","
       (or (cdr (assoc 'unitnumber address)) "") ","
       (cdr (assoc 'town address)) ","
       (nth 1 (cdr (assoc 'province address))) ","
       (cdr (assoc 'postalcode address)) "\n"))
    address-i))


(provide 'test-data)
