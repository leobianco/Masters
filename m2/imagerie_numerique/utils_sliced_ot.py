import numpy as np

def draw_random_direction(d=3):
    """Draws a random vector in S^(d-1) according to the uniform distribution.

    Parameters:
    -----------
    d : int (default is 3)
        Dimension of the vector to be drawn.

    Returns:
    --------
    random_vector : np.array (d)
        Random vector on the sphere, of dimension d.
    """

    random_vector = np.array([np.random.normal() for i in range(d)])
    random_vector /= np.linalg.norm(random_vector)

    return random_vector

def project_image(image, random_direction):
    """Projects all pixels of an image into a random direction.

    Parameters:
    -----------
    image : np.array (h, w, c)
        The image to project
    random_direction : np.array (d)
        The vector of the random direction

    Returns:
    --------
    matrix with dot product between each pixel and direction
        
    """

    # Project all pixels of source into random direction
    rows, cols, nbchannels = image.shape
    projected_image = np.zeros((rows,cols))
    
    for h_i in range(rows):
        for w_i in range(cols):
            p_i = image[h_i, w_i, :]
            proj_p_i = np.dot(p_i, random_direction)
            projected_image[h_i, w_i] = proj_p_i

    return projected_image


def sliced_ot(output, target, n_steps=50):
    """Performs sliced optimal transport.

    Parameters:
    -----------
    output : np.array (b, h, w, c)
        Source / output image.
    Target : np.array (b, h, w, c)
        Target image.
    n_steps : int (default is 50)
        Maximum number of steps (random projections) to perform.

    Returns:
    --------
    ?
    """

    b, rows, cols, nbchannels = output.shape
    step = int(nbchannels/5)

    for i in range(step):
        random_vector = draw_random_direction(nbchannels)
        
        # Project source (output) and target images
        projected_output = project_image(output[0], random_vector)
        projected_target = project_image(target[0], random_vector)
        
        # vectorize matrix
        proj_output = projected_output.reshape(-1)
        proj_target = projected_target.reshape(-1)
        # sort pixels and indexes
        output_order = np.sort(proj_output)
        output_index_order = np.argsort(proj_output)
        target_order = np.sort(proj_target)
        target_index_order = np.argsort(proj_target)

        # Perform advection
        # pass through each position
        for j in range(len(output_order)):
            # get pixel position
            pixel_index_output = output_index_order[j]
            h = int(np.floor(pixel_index_output/cols))
            w = pixel_index_output % cols
            
            # perform advection
            output[0,h, w, :] = output[0,h,w,:] + (target_order[j] - output_order[j]) * random_vector
    
    return output
