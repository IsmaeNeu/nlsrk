dfdt <- function (t, y, param) 
#############################################################################
# Example : the logistic differential equation :                            #
# dy/dt = r y(1-y/k)                                                        #
# r is param[1], k is param[2]                                              #
#############################################################################
{
    param[1] * y * (1 - y/param[2])
}
