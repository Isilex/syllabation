xquery version "1.0" encoding "utf-8";
module namespace local = "http://www.humanitesnumeriques.fr";

(:Naive Approch for french syllabation.      :)
(: By Xavier-Laurent SALVADOR on GitHub      :)
(:-------------------------------------------:)
(:      can be called graal:scande(          :)
(:  "coucou"                                 :)
(:    )                                      :)
(:just contact me at                         :)
(: xavier-laurent.salvador at univ-paris13.fr:)
(:

Result for graal:scande('le héron est savant') is:


<m>
  <p>le</p>
</m>
<m>
  <p>hé</p>
  <p t="tonique">ron</p>
</m>
<m>
  <p t="tonique">est</p>
</m>
<m>
  <p>sa</p>
  <compteDesVers>6</compteDesVers>
  <p t="tonique">vant</p>
</m>



:)

declare function local:syllabator(
  $motif
){

(:la fonction reçoit un mot et renvoie une analyse en syllabe avec la tonique:)   

if (
    string-length(
      $motif
    ) > 1
  ) then
    let $motif := if (
    (
      matches(
        $motif,"[^gqo]ueu"
      )
    )
  ) then replace(
    $motif,"ueu","üeu"
  ) 
                  else $motif 
    let $v:= 
            if (
    matches(
      $motif,"[zrtpqsdfghjklmwxcvbn]y[zrtpqsdfghjklmwxcvbn]"
    )
  ) 
             then (
    "à","a","i","o","ô","ö","u","û","ü","é","è","ê","â","à","î","ï","ë","œ" ,"e","y"
  ) !  string-to-codepoints(.) 
             else (
    "a","i","o","ô","ö","u","û","ü","é","è","ê","â","à","î","ï","ë","œ" ,"e"
  ) !  string-to-codepoints(.)
    
    let $c := if (
    matches(
      $motif,"[aeiouéè]y[aeiouéè]"
    )
  ) 
              then (
    "b","c","ç","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z","y"
  ) !  string-to-codepoints(.)
              else (
    "b","c","ç","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z"
  ) !  string-to-codepoints(.)

    let $str := string-to-codepoints(
    $motif
  )  
    
    let $cons:=
    (:index des groupes consonnantiques:)
    for tumbling window $w in $str
     start $a at $b when (
    $a = $c and (
      $str[$b + 1] = $c
    )
  )
     end $f at $g when (
    $f = $c and $g > $b and not(
      $str[$g + 1] = $c
    )
  )
      where every $q in (
    for $x in (
      $b to $g
    ) return $str[$x]
  ) satisfies $q = $c
     return <p start="{
    $b
  }" end="{
    $g
  }" long="{
    count(
      $w
    )
  }">{
    $w ! (
      <i l="{
        codepoints-to-string(.)
      }">{.}</i>
    )
  }</p>
     
     let $voy:=
     (:index des groupes vocaliques:)
    for tumbling window $w in $str
     start $a at $b when (
    $a = $v and (
      $str[$b + 1] = $v
    )
  )
     end $f at $g when (
    $f = $v and $g > $b and not(
      $str[$g + 1] = $v
    )
  )
      where every $q in (
    for $x in (
      $b to $g
    ) return $str[$x]
  ) satisfies $q = $v
     return 
       <p start="{
    $b
  }" end="{
    $g
  }" long="{
    count(
      $w
    )
  }">{
    $w ! (
      <i l="{
        codepoints-to-string(.)
      }">{.}</i>
    )
  }</p>
     
    
    let $decomp :=
    (:on crée une image consonne + voyelle en extrayant les nasales et les entraves:)
     for $x at $offset in $str 
      return 
         if (
    $cons[@start = $offset]
  ) then 
             if (
    $offset > 1 
                 and 
                 not(
      codepoints-to-string(
        $cons[@start = $offset]/i[1]
      ) = (
        "x","n","m","s","r","l","f","c","t","p"
      )
    )
  ) 
                  then
                   <consonne>{
    $cons[@start = $offset]/i
  }</consonne>
             else if (
    $offset = 1 
                      and codepoints-to-string(
      $cons[@start = $offset]/i[1]
    ) = (
      "v","g","n","m","s","r","l","f","c","t","p","b"
    )
  ) 
                  then 
                   <consonne>{
    $cons[@start = $offset]/i
  }</consonne>
             else if (
    $offset > 1
                      and  codepoints-to-string(
      $cons[@start = $offset]/i[1]
    ) = (
      "c"
    )
                       and codepoints-to-string(
      $cons[@start = $offset]/i[2]
    ) = (
      "h"
    )
  )
                   then <consonne>{
    $cons[@start = $offset]/i
  }</consonne>
             else (
    <voyelle class="gloup">{
      $cons[@start = $offset]/i[1]
    }</voyelle>,<consonne>{
      $cons[@start = $offset]/i[position()>1]
    }</consonne>
  )
             
         else if (
    $voy[@start = $offset]
  ) then
                  if (
    not(
      $voy[@start = $offset]/i ! codepoints-to-string(.) = (
        "ï","ö","ü","ë","ä","é","è"
      )
    )
  ) then
                    <voyelle>{
    $voy[@start = $offset]/i
  }</voyelle>
                  else (
    for $x in $voy[@start = $offset]/i return
                     if (
      codepoints-to-string(
        $x
      ) = (
        "ï","ö","ü","ë","ä"(:,"é","è" PROBLEME lignée / royauté:) 
      )
    )
                      then
                         <voyelle class="trema">{
      $voy[@start = $offset]/i[. = $x]
    }</voyelle>
                       else
                         <voyelle>{
      $voy[@start = $offset]/i[. = $x]
    }</voyelle>
  )
         
         else if (
    (
      $cons,$voy
    )[i[position()>1] = $x and @start < $offset and @end >= $offset]
  ) then () 
         
         else if (
    $x = $c
  ) then <consonne l="{
    codepoints-to-string(
      $x
    )
  }">{
    $x
  }</consonne>
         
         else <voyelle l="{
    codepoints-to-string(
      $x
    )
  }">{
    $x
  }</voyelle>
    
    let $syllabator :=

      for tumbling window $w in $decomp
        start $a when true()
        end $b at $m when (
    (
      name(
        $b
      )="voyelle" and not(
        name(
          $decomp[$m + 1]
        ) = "voyelle"
      )
    ) or (
      $m = count(
        $decomp
      )
    ) or (
      $decomp[$m + 1]/@class="trema"
    ) or (
      $b/@class="trema"
    )
  )
         return <syllabe>{
          string-join(
      for $s in $w//@l/data() return (
        $s
      )
    )
          }</syllabe>
    
    let $resultat :=

    for $syl at $ind in $syllabator
     return 
     if (
    $ind < (
      count(
        $syllabator
      ) - 1
    )
  )
       then 
         if (
    string-length(
      $syl
    ) = 1 and string-length(
      $syllabator[$ind + 1]
    ) = 1
  )
            then (
    <syllabe>{
      concat(
        $syl,$syllabator[$ind + 1]
      )
    }</syllabe>
  )
          else if (
    string-length(
      $syl
    ) = 1 and string-length(
      $syllabator[$ind - 1]
    ) = 1 
  )
            then ()
            else $syl        
       else if (
    (
      string-length(
        $syllabator[last()]
      ) = 1 or not(
        matches(
          $syllabator[last()],"[àaeuioéèôûî]"
        )
      )
    ) and $ind < count(
      $syllabator
    )
  )
         then <syllabe>{
    concat(
      $syl,$syllabator[last()]
    )
  }</syllabe>
       else if (
    not(
      string-length(
        $syllabator[last()]
      ) = 1
    ) and matches(
      $syllabator[last()],"[àaeuioéèôûî]"
    )
  ) 
         then (
    $syl
  )
       else ()
     
    return
    
    (:On calcule la tonique sur la base du e final:)
    
     (
       
     if (
      matches(
        $resultat[last()],"[aeioué]{
          0,2
        }e[s]{
          0,10
        }$",'i'
      )
    )
       then 
         for $x at $ind in $resultat
          return
           if (
      $ind = 1
    ) then
            if (
      matches(
        $x,"^[zrtpqsdfghjklmnwxcvb]"
      )
    ) then
             <syllabe n="{
      $ind
    }" class="{
      if (
        count(
          $resultat
        ) = 2
      ) then 'tonique' else ()
    }" start="consonne">{
      $x/data()
    }</syllabe>
            else 
               <syllabe n="{
      $ind
    }" start="voyelle">{
      $x/data()
    }</syllabe>
           else if (
      $ind = count(
        $resultat
      ) -  1
    )
            then <syllabe n="{
      $ind
    }" class="tonique">{
      $x/data()
    }</syllabe>
           else if (
      $ind = count(
        $resultat
      )
    ) 
            then <syllabe n="{
      $ind
    }" class="feminine">{
      $x/data()
    }</syllabe>
           else <syllabe n="{
      $ind
    }">{
      $x/data()
    }</syllabe>
        else 
         for $x at $ind in $resultat
          return
           if (
      $ind = 1
    ) then
            if (
      matches(
        $x,"^[zrtpqsdfghjklmnwxcvb]"
      )
    ) then
              <syllabe n="{
      $ind
    }" start="consonne">{
      $x/data()
    }</syllabe>
            else 
               <syllabe n="{
      $ind
    }" start="voyelle">{
      $x/data()
    }</syllabe>
           else if (
      $ind = count(
        $resultat
      )
    )
            then <syllabe n="{
      $ind
    }" class="tonique">{
      $x/data()
    }</syllabe>
            else <syllabe n="{
      $ind
    }">{
      $x/data()
    }</syllabe> 
          )
    else  (:le cas de la lettre seule....:)
     if (
    matches(
      $motif,"[zrtpqsdfghjklmwxcvbn]"
    )
  ) then 
        <syllabe n="1" start="consonne">{
    $motif
  }</syllabe>
     else if (
    matches(
      $motif,"[aeiouéèà]"
    )
  ) then  <syllabe n="1" start="voyelle">{
    $motif
  }</syllabe>
     else ()     
};

declare function local:analyzeVers(
  $source
){
let $scansion :=
for $motif in $source 
return <unit>{
 let $m := tokenize(
      $motif,"\W+"
    )[not(
      .=''
    )]
  return 
  for $mm at $ind in $m return
  (
      <mot n="{
        $ind
      }">{
        local:syllabator(
          ft:normalize(
            $mm,map{
              "diacritics":"sensitive"
            }
          )
        )
      }</mot>
    )
  }</unit>
  
  return 
  (
  (:Reste à savoir si les finales se prononcent:)
  (:Donc on teste chaque syllabe:)
  
  for $vers in (
  for $u at $nomb in $scansion
   return <unit n="{
        $nomb
      }">
   {
     for $mot at $ind in $u/mot[not(
          .=''
        )]
     return 
     for $mot in (
     <mot n="{
            $ind
          }">{
      if (
        (
                $mot/syllabe[last()]/@class="feminine" and not(
                  matches(
                    $mot/syllabe[last()],'ée$'
                  )
                ) and not(
                  matches(
                    $mot/syllabe[last()],'s$'
                  )
                ) and $u/mot[$ind + 1]/syllabe[1]/@start="voyelle"
              ) 
        or (
                $mot/syllabe[last()]/@class="feminine" and matches(
                  $u/mot[$ind + 1]/syllabe[1],'^h'
                )
              ) 
        or (
                $mot/syllabe[last()]/@class="feminine" and not(
                  matches(
                    $mot/syllabe[last()],'[^q][ioaéu]e$'
                  )
                ) and $ind = count(
                  $u/mot
                )
              ) 
        and (
                not(
                  matches(
                    $mot/syllabe[last()],'[éè]'
                  )
                )
              )
            )
       then (:e muet:) 
         for $s at $i in $mot/syllabe return 
           if (
              $i = count(
                $mot/syllabe
              )
            ) 
             then <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" silent="oui">{
              $s/data()
            }</syllabe>
           else if (
              $i = 1
            )(:la liaison:)
             then 
               if (
              matches(
                $s/data(),"^[aeiouéèâh]"
              ) and (
                matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'[^e]*[zrtpqsdfghjklmwxcvbn]$'
                ) or matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'.*[zrtpqsdfghjklmwxcvbn]{
                    1,4
                  }e[zrtpqsdfghjklmwxcvbn]{
                    0,4
                  }$'
                ) or  matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'.*qu.*$'
                )
              )
            )
                 then 
                  if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'[^e]*[zrtpqsdfghjklmwxcvbn]$'
              )
            )
                   then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              codepoints-to-string(
                string-to-codepoints(
                  $u/mot[$ind - 1]/syllabe[last()]/data())[last()]
              )
            }">{
              $s/data()
            }</syllabe>
                   else if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*[zrtpqsdfghjklmwxcvbn]{
                  1,4
                }e[zrtpqsdfghjklmwxcvbn]{
                  0,4
                }$'
              )
            ) then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              replace(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'e',''
              )
            }">{
              $s/data()
            }</syllabe>
                   else if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*qu.*$'
              )
            ) then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="k">{
              $s/data()
            }</syllabe>
                   else
                   <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              replace(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*e',''
              )
            }">{
              $s/data()
            }</syllabe>
              else (
              $s
            )
           else $s
       else (:pas e muet:)
         for $s at $i in $mot/syllabe return 
           if (
              $i = count(
                $mot/syllabe
              ) and $i > 1
            ) 
             then <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" silent="non">{
              $s/data()
            }</syllabe>
            else if (
              $i = 1
            )(:la liaison:)
             then 
               if (
              matches(
                $s/data(),"^[aeiouéèâh]"
              ) and (
                matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'[^e]*[zrtpqsdfghjklmwxcvbn]$'
                ) or matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'.*[zrtpqsdfghjklmwxcvbn]{
                    1,4
                  }e[zrtpqsdfghjklmwxcvbn]{
                    0,4
                  }$'
                ) or  matches(
                  $u/mot[$ind - 1]/syllabe[last()]/data(),'.*qu.*$'
                )
              )
            )
                 then 
                  if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'[^e]*[zrtpqsdfghjklmwxcvbn]$'
              )
            )
                   then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              codepoints-to-string(
                string-to-codepoints(
                  $u/mot[$ind - 1]/syllabe[last()]/data())[last()]
              )
            }">{
              $s/data()
            }</syllabe>
                   else if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*[zrtpqsdfghjklmwxcvbn]{
                  1,4
                }e[zrtpqsdfghjklmwxcvbn]{
                  0,4
                }$'
              )
            ) then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              replace(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'e',''
              )
            }">{
              $s/data()
            }</syllabe>
                   else if (
              matches(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*qu.*$'
              )
            ) then
                    <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="k">{
              $s/data()
            }</syllabe>
                   else
                   <syllabe n="{
              $s/@n
            }" class="{
              $s/@class
            }" liaison="{
              replace(
                $u/mot[$ind - 1]/syllabe[last()]/data(),'.*e',''
              )
            }">{
              $s/data()
            }</syllabe>
                 else (
              $s
            )
             else (
              $s
            )
           }</mot>
         )
         return (:la diérèse ouaient / ion / ieur... Le lion tint conseil:)
          <mot n="{
          $mot/@n
        }">{
           for $s in $mot/syllabe
              return <syllabe rese="{
                if (
              matches(
                $s/text(),'[i][aeiouéè]'
              ) or matches(
                $s/text(),'ou[aeiouéè]'
              ) or matches(
                $s/text(),'[^q]u[aei]'
              )
            )
                 then 1
                else () 
              }">{
            $s/@*
          }{
                if (
              matches(
                $s/text(),'[zrtplmqsdfghkwxcvbn][i][aeiouéè][a-z]'
              )
            )
                 then replace(
              $s,'i','I'
            )
                else if (
              matches(
                $s/text(),'ou[aeiouéè]'
              )
            ) 
                 then replace(
              $s,'ou','OU'
            )
               else if (
              matches(
                $s/text(),'[^q]u[aei]'
              )
            )
                 then replace(
              $s,'u(
                [aei]
              )','U$1'
            )
                 else $s/text()
              }</syllabe>
          }</mot>
   }
   </unit>
    )
   
   return 
   let $compteBrut := sum(
      count(
        $vers//syllabe
      )
    ) - (
      count(
        $vers//syllabe[string-length(.) = 1 and not(
          matches(
            .,'[aeioéèïöüàu]'
          )
        )]
      ) + count(
        $vers//syllabe[@silent='oui']
      ) + count(
        $vers//syllabe[. = 'qu']
      )
    )
   let $t := (
      $vers//syllabe[string-length(.) = 1 and not(
        matches(
          .,'[aeioéèïöüàu]'
        )
      )]
    )
   let $u :=  $vers//syllabe[@silent='oui']
    let $v := $vers//syllabe[. = 'qu']
   return 
     <vers n="{
      $vers/@n
    }" cpBrut="{
      $compteBrut
    }" cpRese="{
      $compteBrut + count(
        $vers//syllabe[@rese="1"]
      )
    }">{
       $vers/mot
     }</vers>
 )
};