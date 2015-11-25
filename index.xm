xquery version "1.0" encoding "utf-8";
module namespace graal = "http://www.humanitesnumeriques.fr";

(:Naive Approch for french syllabation.      :)
(: By Xavier-Laurent SALVADOR on GitHub      :)
(:-------------------------------------------:)
(:      can be called graal:scande(          :)
(:  "coucou"                                 :)
(:    )                                      :)
(:just contact me at                         :)
(: xavier-laurent.salvador at univ-paris13.fr:)

declare variable $graal:v_e := (
    "a","i","o","ô","ö","u","û","ü","é","è","ê","â","à","î","ï","ë","œ"
  );
  
declare variable $graal:v := (
  $graal:v_e,"e"
);

declare variable $graal:c:= (
    "b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z","y"
  );
  
declare variable $graal:consonnes_liées:= (
    "c","b","t","g","d","p","k","v"
  );
  
declare variable $graal:consonnes_liantes:= (
    "h","r","l"
  );

declare function graal:contains-any-of (:FunctX:)
  (
   $arg as xs:string? ,
    $searchStrings as xs:string* 
)  as xs:boolean {
   some $searchString in $searchStrings
   satisfies contains(
    $arg,$searchString
  )
 } ;

declare function graal:syllabus(
  $mot
){
     (
      if (
     $mot[1]=$graal:v
   ) 
   then
   <r>{
          $mot[1]
     }</r>
     else (),
     for $i in
  for tumbling window $w in $mot
    start $s at $is when $s=$graal:c or $is>1
    end $e previous $pe next $en when 
        if (
    $pe=$graal:v and $e=$graal:c and $en=$graal:c or $en=" "
  ) then $pe=$graal:v and $e=$graal:c and $en=$graal:c 
        else if (
          (:C'est la partie pour temps/flanc:)
    $pe=$graal:c and $e=$graal:c and $en=$graal:c
  ) then $pe=$graal:c and $e=$graal:c and $en=$graal:c
        else if (
    $pe=$graal:v and $e=$graal:v and $en=$graal:c
  ) then $pe=$graal:v and $e=$graal:v and $en=$graal:c 
        else if (
    $pe=$graal:c and $e=$graal:v and $en=$graal:c
  ) then $pe=$graal:c and $e=$graal:v and $en=$graal:c
        else ()
   return <p>{
    $w
  }</p>
   return 
   <r>{
    replace(
      $i," ",""
    )
  }</r>
    )
};

declare function graal:beautify(
  $resultat
) {
  for $x at $graal:vers in analyze-string(
    replace(
      string-join(
        for $i in $resultat return <p>{
          $i
        }</p>
      )," ",""
    ),"[^\|]*\|"
  ) return 
    for $i in $x//fn:match
      return <p>{
    replace(
      data(
        $i
      ),"\|",""
    )
  }</p>
  };

declare function graal:compte_syllabes(
  $syllabator
){
  (:pour compter les syllabes, on va tester les e muets (
    ie une syllabe qui se termine par un e muet et suivi d'une voyelle
  ). On fait ensuite nb de syllabes - ce total.:)
  for $graal:vr at $l in $syllabator
              return 
              <v ligne="{
  $l
}">{
                count(
    $graal:vr/mot/p
  ) -
                 sum(
    for $m at $i in $graal:vr/mot return 
                 (
      if (
        $m/p[last()][not(
          ./@t
        )] and graal:contains-any-of(
            substring(
            $graal:vr/mot[$i+1]/p[1],1,1
          ),$graal:v
          )
      or $m/p[last()][not(
            ./@t
          )] and not(
            $graal:vr/mot[$i+1]
          )
        ) then 1 else ())
  )
       }</v>
};

declare function graal:emuet(
  $res
){
   if (
  count(
    $res//p
  )<2
  and graal:contains-any-of(
      $res/p,$graal:v_e
    )
) then <mot><p t="tonique">{
  data(
    $res/p
  )
}</p></mot>
  else if (
  count(
    $res//p
  )<2
  and not(
        graal:contains-any-of(
        $res/p,$graal:v
      )
    )
) then <mot><p>{
  data(
    $res/p
  )
}</p></mot>
  else 
    for $p at $i in $res
    let $last := count(
  $p//p
)
    return
    if (
      (:Les cas où c'est l'avant-dernière la tonique:)
  matches(
    $p//p[position()=$last],"[^u]e[^srzxncm]","i"
  ) and not(
     graal:contains-any-of(
        $p//p[position()=$last],$graal:v_e
      )
   )
  or
  matches(
    $p//p[position()=$last],"e$","i"
  ) and not(
    graal:contains-any-of(
        $p//p[position()=$last],$graal:v_e
      )
  ) 
  or
  matches(
    $p//p[position()=$last],"[q|g]ue[s]?$","i"
  ) 
  (:problème de que et gue en fin de mot:) 
) then 
    <mot>{
      for $syll at $j in $p//p
      return
      if (
    $j=$last - 1
  ) then 
       <p t="tonique">{
    data(
      $syll
    )
  }</p> 
      else $syll
  }</mot>
  else
  <mot>{
      for $syll at $j in $p//p
      return
      if (
    $j=$last
  ) then 
        <p t="tonique">{
    data(
      $syll
    )
  }</p> 
      else $syll
  }</mot>  
};


declare function graal:scande(
  $input as xs:string
) {
let $graal:vers := $input


let $syllabator :=
for $x in tokenize(
  lower-case(
    replace(
      $graal:vers,"'",""
    )
  ),"\n"
)
return
<vers>{
  for $graal:cc at $p in tokenize(
  $x,"\W+"
)[not(
  .=""
)]               
    return
  
  for $res in
  <res>{
  let $res := graal:syllabus(
      tokenize(
    replace(
      $graal:cc,"(.)","$1;"
    ),";"
  )
    )
     
  let $graal:cseule := 
  for $x at $i in $res return 
  if (
    string-length(
      $x
    )<2
  ) then $i else ()
  
  let $resultat:=
  for $x at $index in $res
    let $graal:concat := if (
    $index+1=$graal:cseule
  ) then () else "|"
    return
     if (
      $x=$graal:consonnes_liées and substring(
        $res[$index+1],1,1
      )= $graal:consonnes_liantes
    ) then 
     string-join(
    $graal:concat||data(
      $x
    )
  ) 
    else 
    string-join(
    data(
      $x
    )||$graal:concat
  )
  
  return
  
   for $x in graal:beautify(
        $resultat
      )
     return 
     if (
        matches(
          $x,"[^aeiou]y$"
        )
      )
      then (
        element p {
          replace(
            $x,".y",""
          )
        },element p {
          replace(
            $x,"(
              .*
            )(
              .y
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,"pay[s]?"
        )
      )
      then (
        element p {
          "pa"
        },element p {
          replace(
            $x,"(
              .*
            )(
              y[s]?
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,"[aoéè][éè]"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              [^éè]*
            )(
              [éè].*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              [^éè]*
            )(
              [éè].*
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,".*[éè][aeiou]"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              .*[éè]
            )(
              .*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              .*[éè]
            )(
              .*
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,"ï"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              [^ï]*
            )(
              [ï].*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              [^ï]*
            )(
              [ï].*
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,"ü"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              [^ü]*
            )(
              [ü].*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              [^ü]*
            )(
              [ü].*
            )","$2"
          )
        }
      )
     else if (
        matches(
          $x,"xys"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              [^x]*
            )(
              [x].*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              [^x]*
            )(
              [x].*
            )","$2"
          )
        }
      )
	 else if (
        matches(
          $x,"[^q]ue[unr]"
        )
      )
      then (
        element p {
          replace(
            $x,"(
              .*u
            )(
              e.*
            )","$1"
          )
        },element p {
          replace(
            $x,"(
              .*u
            )(
              e.*
            )","$2"
          )
        }
      )
      else $x
    
  }</res>

return 
 graal:emuet(
    $res
  )
}
</vers>

let $nb := count(
  $syllabator
)
 
let $type := graal:compte_syllabes(
  $syllabator
)

let $dierese := for $x at $im in $syllabator return 
                <vers>{
    for $y in $x/mot return 
                     <mot>{
                       (
        for $z in $y/p return 
                       if (
          not(
            $type[./@ligne=$im]=12
          ) and matches(
            $z,"io"
          ) or not(
            $type[./@ligne=$im]=10
          ) and matches(
            $z,"io"
          ) or matches(
            $z,"io"
          ) and not(
            $type[./@ligne=$im]=8
          )
        )
                       then 
                       (
          element p {
            replace(
              $z,"(
                [^i]*i
              )o.*","$1"
            )
          },
                       element p {
            $z/@*, replace(
              $z,"[^i]*i(
                o.*
              )","$1"
            )
          }
        )
                       else $z
                       )
                     }</mot>
  }
                     </vers>
      
return

  for $graal:ve at $i in $dierese 
    let $echotype := for $typex in $type[./@ligne=$i] return if (
      $typex=12
    ) then "alexandrin" else if (
      $typex=8
    ) then "octosyllabe" else if(
      $typex=10
    ) then "décasyllabe" 
    else if (
    $typex=11 or $typex=13
  )
    then "alexandrin"
       else if (
    $typex=9
  )
    then "decasyllabe"  
        else if (
    $typex=7
  )
    then "octosyllabe"  
    else "prose ?"||$typex
  
  
  return 
      (
     for $unit at $imot in
     <corps>{
      for $m at $i in $graal:ve/mot return
      <m>{
         
          (
            (:On va d'abord se demander s'il y a un e muet à mettre entre parenthèses:)
        if (
          $m/p[last()][not(
            ./@t
          )] and 
          graal:contains-any-of(
                substring(
                  $graal:ve/mot[$i+1]/p[1],1,1
                ),(:$graal:v,"h" Le H MUET !!!!:)$graal:v
              )
              
           (:Le H muet !:)
          or $m/p[last()][not(
            ./@t
          )] and matches(
              data(
                $graal:ve/mot[$i+1]
              ),"^homm"
            )
          
          or $m/p[last()][not(
            ./@t
          )] and matches(
              data(
                $graal:ve/mot[$i+1]
              ),"^h[^aé]"
            ) 
          
          or $m/p[last()][not(
            ./@t
          )] and matches(
              data(
                $graal:ve/mot[$i+1]
              ),"^héc"
            )
          
          or $m/p[last()][not(
            ./@t
          )] and not(
              $graal:ve/mot[$i+1]
            )
        ) 
            then for $p at $ix in $m/p return 
                if (
            $ix = count(
              $m/p
            )
          ) then 
          if (
            substring(
              $p,string-length(
                $p
              ),1
            ) = $graal:c and 
            graal:contains-any-of(
                substring(
                  $graal:ve/mot[$i+1]/p[1],1,1
                ),(
                $graal:v
              )
              )
          )
          (:liaison consonnantique:)
          then          
          (
            <p t="emuette" liaison="{
              (
                substring(
                  $p,string-length(
                    $p
                  ),1
                )
              )
            }">{
          replace(
            $p,"e","(e)"   
          )
        }</p>
          ) 
        else <p t="emuette" liaison="{
              (
                substring-before(
                  $p,'e'                  
                )
              )
            }">{
          (:pas de liaison consonnantique:)
          replace(
            $p,"e","(e)"
          )
        }</p>
        
         else $p
         
        else for $p at $ix in $m/p 
          return 
           if (
          $ix=$m/p/last() and substring(
              $p,string-length(
                $p
              ),1
            ) = $graal:c and graal:contains-any-of(
                substring(
                  $graal:ve/mot[$i+1]/p[1],1,1
                ),$graal:v
              )
          )
           then          
          
           element {
             node-name(
                $p
              )
               }             
            {
              attribute liaison {
            (
              substring(
                $p,string-length(
                  $p
                ),1
              )
            )
              },
         $p/@*,
            data(
              $p
            )
      }
        
          else $p
      )  
      }</m>
    }</corps>
      return        
          for $x at $imot in $unit/m     
          return
          if (
        $unit/m[$imot - 1]//@liaison
      )
          then 
          <m>{
            (
        $x/@*,<l>{
				if (
            $unit/m[$imot - 1]//@liaison="d" and matches(
              $unit/m[$imot - 1]//p[last()],"d$","i"
            )
          )  
				 then "t" 
				else if (
            $unit/m[$imot - 1]//@liaison="s"
          )
				 then "z" 
				else if (
            $unit/m[$imot - 1]//@liaison="v" and matches(
              $unit/m[$imot - 1]//p[last()],"v$","i"
            )
          )
				 then "f"
				else if (
            $unit/m[$imot - 1]//@liaison="b" and matches(
              $unit/m[$imot - 1]//p[last()],"b$","i"
            )
          )
				 then "p"
				else if (
            $unit/m[$imot - 1]//@liaison="g" and matches(
              $unit/m[$imot - 1]//p[last()],"g$","i"
            )
          )
				 then "k"  
				else 
				 data(
            $unit/m[$imot - 1]//@liaison
          )
				}</l>,
            for $p at $graal:cp in $x/p 
            let $numsyll := count(
          for $w in (
            1 to $imot - 1
          ) return (
            $unit/m[$w]/p
          )
        )+ $graal:cp 
            return (
              if(
            $echotype="alexandrin" and $numsyll<7 and $numsyll>5
          ) 
				then 
				(
            $p,<cut>|</cut>
          ) 
				else if (
            not(
              $unit/m[$imot]/p[$graal:cp+1]
            ) and not(
              $unit/m[$imot + 1]
            ) 
          ) then (
            <compteDesVers>{
              if (
                $numsyll - count(
                  $unit//p[@t="emuet"]
                )=12 or $numsyll - count(
                  $unit//p[@t="emuet"]
                )=13
              )  then "alexandrin" else if (
                $numsyll - count(
                  $unit//p[@t="emuet"]
                )=8
              ) then "octosyllabe" else if (
                $numsyll - count(
                  $unit//p[@t="emuet"]
                )=10
              ) then "décasyllabe" else $numsyll - count(
                $unit//p[@t="emuet"]
              )
            }</compteDesVers>,$p
          ) 
				else 
					$p
        )
      )				
          }</m>
          else  
          <m>{
          for $p at $graal:cp in $x/p 
          let $numsyll := count(
        for $w in (
          1 to $imot - 1
        ) return (
          $unit/m[$w]/p
        )
      )+ $graal:cp
          return 
           if(
        $echotype="alexandrin" and $numsyll<7 and $numsyll>5
      ) 
            then 
				(
        $p,<cut>|</cut>
      )
			else if (
        not(
          $unit/m[$imot]/p[$graal:cp+1]
        ) and not(
          $unit/m[$imot + 1]
        ) 
      ) then (
        <compteDesVers>{
          if (
            $numsyll - count(
              $unit//p[@t="emuet"]
            )=12 or $numsyll - count(
              $unit//p[@t="emuet"]
            )=13
          ) then "alexandrin" else if (
            $numsyll - count(
              $unit//p[@t="emuet"]
            )=8
          ) then "octosyllabe" else if (
            $numsyll - count(
              $unit//p[@t="emuet"]
            )=10
          ) then "décasyllabe" else $numsyll - count(
            $unit//p[@t="emuet"]
          )
        }</compteDesVers>,$p
      ) 
            else 
				(
        $p
      )
          }</m>
    )
};