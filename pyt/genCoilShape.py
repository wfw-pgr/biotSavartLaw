import numpy as np

# ========================================================= #
# ===  generate Coil Shape Information                  === #
# ========================================================= #
def genCoilShape():
    # ------------------------------------------------- #
    # --- [1] Parameters                            --- #
    # ------------------------------------------------- #
    xpos        = 0.0
    ypos        = 0.0
    zpos        = 0.0
    radius      = 1.0
    nTheta      = 1001
    outFile     = "dat/coilShape.dat"
    # ------------------------------------------------- #
    # --- [2] make Coil Shape                       --- #
    # ------------------------------------------------- #
    theta       = np.linspace( 0.0, 2.0*np.pi, nTheta )
    wData       = np.zeros( (nTheta,3) )
    wData[:,0]  = xpos + radius * np.cos( theta )
    wData[:,1]  = ypos + radius * np.sin( theta )
    wData[:,2]  = zpos
    # ------------------------------------------------- #
    # --- [3] write Data into a File                --- #
    # ------------------------------------------------- #
    with open( outFile, "w" ) as f:
        f.write( "# x y z\n" )
        np.savetxt( f, wData )

    
# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    genCoilShape()
