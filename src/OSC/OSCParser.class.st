"
OSCParser is a message parser.
Its main method is parse: which returns a collection of messages.
"
Class {
	#name : #OSCParser,
	#superclass : #Object,
	#instVars : [
		'byteStream',
		'message',
		'types'
	],
	#classVars : [
		'BundleStr'
	],
	#category : #'OSC-Kernel'
}

{ #category : #'class initialization' }
OSCParser class >> initialize [
	"just to get a null-terminated string, which is how OSC goodness arrives..."
	BundleStr := ('#bundle' nullTerminatedEncodeWith: #utf8) asString.
]

{ #category : #parse }
OSCParser class >> parse: aByteStream [
	| messageColl messages |
	messageColl := OrderedCollection new.
	[aByteStream atEnd] whileFalse: [
		messages := (self new initializeFor: aByteStream) parse.
		messages ifNotNil: [
			messageColl addAll: messages
		]
	].
	^messageColl
]

{ #category : #initialization }
OSCParser >> initializeFor: aByteStream [
	byteStream := aByteStream.
	"message := OrderedCollection new."
	"types := OrderedCollection new."
	"the foregoing two now happen at the top of each invocation of #parseOneMessage"

]

{ #category : #parsing }
OSCParser >> parse [
	| firstChar |
	firstChar := byteStream peek asCharacter.

	firstChar = $/
		ifTrue: [ ^ self parseOneMessage ].

	(firstChar = $#
		and: [ (byteStream next: 8) asString = BundleStr ])
			ifTrue: [ ^ self parseBundle ].

	^ Error new signal: 'mangled OSC packet'
]

{ #category : #'element-parsing' }
OSCParser >> parseBlob [
	^ nil
]

{ #category : #parsing }
OSCParser >> parseBundle [
	| messageColl subParses timeTag bunSize |

	timeTag := byteStream next: 8.
  "for the moment we're not doing anything with the time tag..."
	messageColl := OrderedCollection new.

	[byteStream atEnd] whileFalse: [
		bunSize := byteStream nextInt32.
		subParses := (self class new initializeFor: (byteStream next: bunSize) readStream) parse.
		messageColl addAll: subParses.
	].

	^ messageColl
]

{ #category : #'element-parsing' }
OSCParser >> parseFalse [
	message add: false
]

{ #category : #'element-parsing' }
OSCParser >> parseFloat32 [
	message add: (Float fromIEEE32Bit: (byteStream binary nextNumber: 4))
]

{ #category : #'element-parsing' }
OSCParser >> parseFloat64 [
	message add: (Float fromIEEE64Bit: (byteStream binary nextNumber: 8))
]

{ #category : #parsing }
OSCParser >> parseHeader [
	|header|
	header := (byteStream upTo: $, asInteger) copyWithout: 0.
	message add: (ZnCharacterEncoder ascii decodeBytes: header).

]

{ #category : #'element-parsing' }
OSCParser >> parseInt32 [
	message add: byteStream nextInt32
]

{ #category : #'element-parsing' }
OSCParser >> parseInt64 [
	message add: byteStream nextInt64
]

{ #category : #parsing }
OSCParser >> parseNextValueTyped: aType [
	aType = 105 "i" ifTrue: [^self parseInt32].
	aType = 102 "f" ifTrue: [^self parseFloat32].
	aType = 115 "s" ifTrue: [^self parseString].
	aType = 100 "d" ifTrue: [^self parseFloat64].
	aType = 104 "h" ifTrue: [^self parseInt64].
	aType = 84  "T" ifTrue: [^self parseTrue].
	aType = 70  "F" ifTrue: [^self parseFalse].
	aType = 78  "N" ifTrue: [^self parseNil].
	aType = 98  "b" ifTrue: [^self parseBlob]


]

{ #category : #'element-parsing' }
OSCParser >> parseNil [
	message add: nil
]

{ #category : #parsing }
OSCParser >> parseOneMessage [
	message := OrderedCollection new.
	types := OrderedCollection new.
	self
		parseHeader;
		parseTypes;
		parseValues.
	^ (OrderedCollection new) add: message asArray; yourself
]

{ #category : #'element-parsing' }
OSCParser >> parseString [
	message add: (ZnCharacterEncoder ascii decodeBytes: (byteStream upTo: 0)).
	(4 - (byteStream position)) \\ 4 timesRepeat: [ byteStream next ]
]

{ #category : #'element-parsing' }
OSCParser >> parseTrue [
	message add: true
]

{ #category : #'element-parsing' }
OSCParser >> parseTypes [
	| next |
	byteStream atEnd ifTrue: [^self].
	next := byteStream next.
	next = 0 ifTrue: [
		((4 - (byteStream position \\ 4)) \\ 4) timesRepeat: [byteStream next].
		^self
	].
	  "i   f   s   d   h   T  F  N  b"
	(#[105 102 115 100 104 84 70 78 98] includes: next) ifTrue: [types add: next].
	self parseTypes.
]

{ #category : #parsing }
OSCParser >> parseValues [
	| aTypeStream |
	aTypeStream := ReadStream on: types.
	[aTypeStream atEnd] whileFalse: [self parseNextValueTyped: aTypeStream next]
]
