import sys
import numpy
output="planardev.txt"
target=open(output,'w')
vec=[0.55735026919,0.55735026919,0.55735026919]

L=12
for c in range(1,len(sys.argv)):
    input=sys.argv[c]
    mA=[]
    mB=[]
    mC=[]
    mD=[]
    mE=[]
    mF=[]

    sA=[]
    sB=[]
    sC=[]
    sD=[]
    sE=[]
    sF=[]

    for i in range(0,3):
        mA.append(0)
        mB.append(0)
        mC.append(0)
        mD.append(0)
        mE.append(0)
        mF.append(0)

        sA.append(0)
        sB.append(0)
        sC.append(0)
        sD.append(0)
        sE.append(0)
        sF.append(0)

    if (L == 2):
        per=1
        rep=1
    elif (L == 6):
        per=9
        rep=3
    elif (L == 12):
        per=36
        rep=6
    elif (L== 18):
        per=81
        rep=9

    n=0
    #Calculate average of each spin
    #A and D
    with open(input) as f:
        data = f.readlines()
        for i in range(0,rep):
            #Sum up A spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mA[k]=mA[k] + float(line[k])
            #Sum up D spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mD[k]=mD[k] + float(line[k])
        for i in range(0,rep):
            #Sum up E spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mE[k]=mE[k] + float(line[k])
            #Sum up B spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mB[k]=mB[k] + float(line[k])
        for i in range(0,rep):
            #Sum up C spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mC[k]=mC[k] + float(line[k])
            #Sum up F spin
            for j in range(0,per):
                line=data[n].split()
                n=n+1
                for k in range(0,3):
                    mF[k]=mF[k] + float(line[k])
    
    div=rep*per

    mA[:] = [x / div for x in mA]
    mB[:] = [x / div for x in mB]
    mC[:] = [x / div for x in mC]
    mD[:] = [x / div for x in mD]
    mE[:] = [x / div for x in mE]
    mF[:] = [x / div for x in mF]
    
    dotA=numpy.dot(mA,vec)
    dotB=numpy.dot(mB,vec)
    dotC=numpy.dot(mC,vec)
    dotD=numpy.dot(mD,vec)
    dotE=numpy.dot(mE,vec)
    dotF=numpy.dot(mF,vec)
    
    target.write("%s %s %s %s %s %s" % (dotA,dotB,dotC,dotD,dotE,dotF))
    target.write("\n")
target.close()
