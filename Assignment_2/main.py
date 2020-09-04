import numpy
import sys
from copy import deepcopy
from PIL import Image, ImageDraw, ImageChops, ImageFont
import random
import time

start_time = time.time()
input = '/input/Fighter.jpg'
output = '/output/Fighter_output.png'

# input image
im = Image.open(input).convert('RGBA')
input_image = im.resize((512,512))  # to ensure that image size is 512x512
input_image.show()
# input_image.save(input)

#population size
population_size = 20

#min difference between input and output images
fitness = 1000

# number of generations
generation = 5000

# initialization of best and second best individuals and best fitness
best = None
second_best = None
best_fitness = sys.float_info.max


# fitness function (root mean square difference)
def fitness_func(image):
    rms = ImageChops.difference(image, input_image)
    histogram = rms.histogram()
    squares = (value * ((index % 256) ** 2) for index, value in enumerate(histogram))
    squares_sum = numpy.sum(squares)
    answer = squares_sum / float(512 * 512)

    return int(answer)


# mutation function
def mutation(image):
    txt = "Alisa"
    fontsize = 16
    font = ImageFont.truetype("Inkfree.ttf", fontsize)
    canva = ImageDraw.Draw(image)

    for i in range(0, 3):
        color = numpy.random.randint(0, 256, 3)
        color = (color[0], color[1], color[2], 255)
        x = random.randint(-3, 508)
        y = random.randint(-3, 508)

        canva.text((x,y), txt, fill = color, font = font)

    return image


# crossover function - blending two images
def crossover(image1, image2):
    return Image.blend(image1, image2, 0.5)


# selection function
def selection(population):
    global best
    global second_best
    global best_fitness

    #find best individual
    for individual in population:
        current_fitness = fitness_func(individual)

        if best is None or fitness_func(individual) <= best_fitness:
            second_best = best
            best = individual
            best_fitness = current_fitness

    # creating new population
    new_population = list()

    new_population.append(best)
    new_population.append(second_best)

    population.remove(best)
    if second_best in population:
        population.remove(second_best)

    # filling new population
    for individual in population:
        new_population.append(mutation(deepcopy(crossover(best, second_best))))

    return new_population


# learning function
def genetic_algorithm(fitness):
    global best
    global best_fitness

    population = list()

    cash_image = Image.new('RGBA', (512, 512), (0, 0, 0, 255))

    # filling population
    for i in range(population_size):
        population.append(deepcopy(cash_image))

    for i in range(generation):
        best_fitness = sys.float_info.max

        population = selection(population)

        # printing number of iteration, save intermediate result
        if i % 100 == 0:
            best.save(output)
            print('Iteration: ' + str(i))

            # checking fitness
            print(best_fitness)
            if best_fitness < fitness:
                return

    # saving output image
    best.save(output)


# running genetic algorithm
genetic_algorithm(fitness)

end = time.time() - start_time
end /= 60.0
print('%.3f' % end + ' min.')
