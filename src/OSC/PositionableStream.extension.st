Extension { #name : #PositionableStream }

{ #category : #'*OSC' }
PositionableStream >> nextInt64 [
	"Read a 64-bit signed integer from the next eight (8) bytes"

	| s |
	s := 0.
	1 to: 8 do: [ :i | s := (s bitShift: 8) + self next ].
	^ (s bitAnd: 16r8000000000000000) = 0
		ifTrue: [ s ]
		ifFalse: [ -1 - s bitInvert64 ]
]
