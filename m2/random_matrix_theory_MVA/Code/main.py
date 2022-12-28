import simulations


def main():
    choice = input("Enter a simulation to run: ")
    simulation_string = 'simulation_'+choice
    simulation_method = getattr(simulations, simulation_string)
    # Call the simulation
    simulation_method()


if __name__=="__main__":
    main()
