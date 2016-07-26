From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:04:55 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08126; Thu, 2 Jun 88 14:04:54 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04336; Thu, 2 Jun 88 13:30:53 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08160; Thu, 2 Jun 88 12:46:20+0900
Date: Thu, 2 Jun 88 12:46:20+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08160@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: rtemp.l.frl
Status: RO


(include declar)
(fassert sensor-a
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-a sensor-stack)

(fassert sensor-b
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))
   
(push 'sensor-b sensor-stack)

(fassert sensor-c
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-c sensor-stack)

(fassert sensor-d
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-d sensor-stack)

(fassert sensor-e
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-e sensor-stack)

(fassert sensor-f
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-f sensor-stack)

(fassert sensor-g
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-g sensor-stack)

(fassert sensor-h
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-h sensor-stack)

(fassert sensor-i
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-i sensor-stack)

(fassert sensor-j
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-j sensor-stack)

(fassert sensor-k
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-k sensor-stack)

(fassert sensor-l
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-l sensor-stack)

(fassert sensor-m
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-m sensor-stack)

(fassert sensor-n
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-n sensor-stack)

(fassert sensor-o
    (ako ($value (sensor)))
    (frame)
    (slot)
    (sentinel)
    (set)
    (type)
    (test))

(push 'sensor-o sensor-stack)

(fassert sentinel-a
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-a sentinel-stack)

(fassert sentinel-b
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-b sentinel-stack)

(fassert sentinel-c
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-c sentinel-stack)

(fassert sentinel-d
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-d sentinel-stack)

(fassert sentinel-e
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-e sentinel-stack)

(fassert sentinel-f
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-f sentinel-stack)

(fassert sentinel-g
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-g sentinel-stack)

(fassert sentinel-h
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-h sentinel-stack)

(fassert sentinel-i
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-i sentinel-stack)

(fassert sentinel-j
    (ako ($value (sentinel)))
    (body)
    (slot)
    (type)
    (group))

(push 'sentinel-j sentinel-stack)

(fassert and-sensor-a
    (ako ($value (and-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'and-sensor-a and-sensor-stack)

(fassert and-sensor-b
    (ako ($value (and-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'and-sensor-b and-sensor-stack)

(fassert and-sensor-c
    (ako ($value (and-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'and-sensor-c and-sensor-stack)

(fassert and-sensor-d
    (ako ($value (and-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'and-sensor-d and-sensor-stack)

(fassert and-sensor-e
    (ako ($value (and-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'and-sensor-e and-sensor-stack)

(fassert or-sensor-a
    (ako ($value (or-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'or-sensor-a or-sensor-stack)

(fassert or-sensor-b
    (ako ($value (or-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'or-sensor-b or-sensor-stack)

(fassert or-sensor-c
    (ako ($value (or-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'or-sensor-c or-sensor-stack)

(fassert or-sensor-d
    (ako ($value (or-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'or-sensor-d or-sensor-stack)

(fassert or-sensor-e
    (ako ($value (or-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'or-sensor-e or-sensor-stack)

(fassert in-sensor-a
    (ako ($value (in-sensor)))
    (group)
    (sentinel)
    (place)
    (set)
    (type))

(push 'in-sensor-a in-sensor-stack)

(fassert in-sensor-b
    (ako ($value (in-sensor)))
    (group)
    (sentinel)
    (place)
    (set)
    (type))

(push 'in-sensor-b in-sensor-stack)

(fassert in-sensor-c
    (ako ($value (in-sensor)))
    (group)
    (sentinel)
    (place)
    (set)
    (type))

(push 'in-sensor-c in-sensor-stack)

(fassert in-sensor-d
    (ako ($value (in-sensor)))
    (group)
    (sentinel)
    (place)
    (set)
    (type))

(push 'in-sensor-d in-sensor-stack)

(fassert in-sensor-e
    (ako ($value (in-sensor)))
    (group)
    (sentinel)
    (place)
    (set)
    (type))

(push 'in-sensor-e in-sensor-stack)

(fassert group-sensor-a
    (ako ($value (group-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'group-sensor-a group-sensor-stack)

(fassert group-sensor-b
    (ako ($value (group-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'group-sensor-b group-sensor-stack)

(fassert group-sensor-c
    (ako ($value (group-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'group-sensor-c group-sensor-stack)

(fassert group-sensor-d
    (ako ($value (group-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'group-sensor-d group-sensor-stack)

(fassert group-sensor-e
    (ako ($value (group-sensor)))
    (group)
    (sentinel)
    (set)
    (type))

(push 'group-sensor-e group-sensor-stack)





