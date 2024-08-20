\version "2.25.18"
\include "collaparte.ily"

music = \relative {
  c'4 d e f g a b c
  %  \new Voice = "4" { g4 4 4 4 }
  \voices 1,2 <<
    {
      \resetRelativeOctave c''
      c4 d e f g a b c
      d4 c b a g f e \parenthesize d
      \once\override NoteHead.color = #red
      c1
    }
    \\
    {
      g1
    }
  >>
  <<
    {
      c,1
    }
    \context Staff = "VlnI_div" {
      c'2 2
    }
  >>
  \voices 1,2 <<
    {
      \resetRelativeOctave c'
      c8 e d f e g f a
      g b a c b d c e
      d4 c b a g f e d
      c1
    }
    \\
    {
      g1
    }
  >>
  f'1(\<
  g2 a)\f
}

global = {
  s1*15
}

\collaParte \new StaffGroup = "VlnI" \with {
  instrumentName = "Violin I"
  shortInstrumentName = "Vln. I"
} <<
  \new StaffGroup = "VlnI_soli" <<
    \new Staff = "VlnI_solo_non_colla" \with {
      collaParteRelays = ##f
    } \global
    \new Staff = "VlnI_solo_colla" { s1*14 e'2 d')\f }
  >>
  \new Staff = "VlnI" \with {
    \collaParteSource
  } << \global \music >>
  \new Staff = "VlnI_div" \global
>>

