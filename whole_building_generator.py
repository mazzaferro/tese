# RUN USING ONLY Python v2.7!!!
# -*- coding: utf-8 -*-


import pyidf as pf
pf.validation_level = pf.ValidationLevel.no
import logging
logging.info("start")
from pyidf.idf import IDF

import pandas as pd
import os
import sys

def main(shell_depth = 5, 
	ceiling_height = 3, 
	building_xlen = 20, 
	building_ratio = 1, 
	wwr = .25, 
	shading = 0, 
	azimuth = 0, 
	nfloor = 10, 
	pilotis = 0, 
	floor_construction = "PisoCeramica", 
	roof_construction = "Cobertura22", 
	extwall_construction = "Parede22", 
	glass_construction = "Vid2", 
	schedule = "SCH_08H", 
	people = 0.0833, 
	lights = 10., 
	equip = 10.,  
	abs_roof_construction = 0.5, 
	abs_extwall_construction = 0.6, 
	shading_ceiling_height = "1", 
	sur_nfloor = 5, 
	sur_abs_extwall = 0.4, 
	sur_glass = "Vid2", 
	sur_wwr = 0.5, 
	horizontal_building_density = 0.5, 
	building_block_position = 12, 
	sur_vis_transmittance = 1, 
	thermostat = 24, 
	output = 'output.idf', 
	path=os.getcwd(), 
	input = "seed.idf"):
	
	os.chdir(path)
	idf=IDF(input)

	# Passar as variaveis numericas pra float soh pra garantir
	shell_depth = float(shell_depth)
	building_xlen = float(building_xlen)
	building_ratio = float(building_ratio)
	building_ylen = building_xlen*building_ratio
	ceiling_height = float(ceiling_height)
	azimuth = float(azimuth)
	wwr = float(wwr)
	shading = float(shading)
	nfloor = int(nfloor)
	pilotis = int(pilotis)
	adiabatic_floor_model = 0
	
	#variaveis dependentes

	window_z1 = ceiling_height*(1-wwr)/2
	window_z2 = window_z1+(ceiling_height*wwr)
	window_x1 = building_xlen*.001
	window_x2 = building_xlen*.999
	window_y1 = building_ylen*.001
	window_y2 = building_ylen*.999

	#Lista das Zones
	
	Zones = dict()
	Zones['Name'] = [] # 'PZ1','PZ2','PZ3','PZ4','CORE'
	Zones['X'] = [] # [0,0,building_xlen,building_xlen,shell_depth]
	Zones['Y'] = [] # [building_ylen,0,0,building_ylen,shell_depth]
	Zones['Z'] = [] # [hpav,hpav,hpav,hpav,hpav]

	#Surfaces

	BldgSurface = dict()
	BldgSurface['Name'] = []
	BldgSurface['SurfaceType'] =[]
	BldgSurface['ConstructionName'] = []
	BldgSurface['ZoneName'] = []
	BldgSurface['OutsideBoundaryCond'] = []
	BldgSurface['OutsideBoundryCondObj'] = []
	BldgSurface['SunExposure'] = []
	BldgSurface['WindExposure'] = []
	BldgSurface['V1x'] = []
	BldgSurface['V1y'] = []
	BldgSurface['V1z'] = []
	BldgSurface['V2x'] = []
	BldgSurface['V2y'] = []
	BldgSurface['V2z'] = []
	BldgSurface['V3x'] = []
	BldgSurface['V3y'] = []
	BldgSurface['V3z'] = []
	BldgSurface['V4x'] = []
	BldgSurface['V4y'] = []
	BldgSurface['V4z'] = []
		
	FenSurface = dict()
	FenSurface['Name'] = []
	FenSurface['SurfaceType'] = []
	FenSurface['ConstructionName'] = []
	FenSurface['BuildingSurfaceName'] = []
	FenSurface['OutsideBoundryCondObj'] = []
	FenSurface['ShadingControlName'] = []
	FenSurface['V1x'] = []
	FenSurface['V1y'] = []
	FenSurface['V1z'] = []
	FenSurface['V2x'] = []
	FenSurface['V2y'] = []
	FenSurface['V2z'] = []
	FenSurface['V3x'] = []
	FenSurface['V3y'] = []
	FenSurface['V3z'] = []
	FenSurface['V4x'] = []
	FenSurface['V4y'] = []
	FenSurface['V4z'] = []

	# Shading:Building:Detailed

	ShadingBuildingDetailed = {}
	ShadingBuildingDetailed['Name'] = []
	ShadingBuildingDetailed['Transmittance Schedule Name'] = []
	ShadingBuildingDetailed['Number of Vertices'] = []
	ShadingBuildingDetailed['V1x'] = []
	ShadingBuildingDetailed['V1y'] = []
	ShadingBuildingDetailed['V1z'] = []
	ShadingBuildingDetailed['V2x'] = []
	ShadingBuildingDetailed['V2y'] = []
	ShadingBuildingDetailed['V2z'] = []
	ShadingBuildingDetailed['V3x'] = []
	ShadingBuildingDetailed['V3y'] = []
	ShadingBuildingDetailed['V3z'] = []
	ShadingBuildingDetailed['V4x'] = []
	ShadingBuildingDetailed['V4y'] = []
	ShadingBuildingDetailed['V4z'] = []


	# Prepare for iteration
	
	if pilotis == 1:
		if nfloor == 1:
			boundaries = 'pex'
		else:
			boundaries = 'pad'
	else:
		if nfloor == 1:
			boundaries = 'exp'
		else:
			boundaries = 'bot'
		
	for floor in range(nfloor):
	
		hpav = floor*ceiling_height
		# check boundary condition
		if floor > 0:
			boundaries = 'mid'
		if floor == nfloor-1 and nfloor > 1:
			boundaries = 'top'
		
		Zones['Name'].append('PZ1_' + str(floor + 1))
		Zones['X'].append(0)
		Zones['Y'].append(building_ylen)
		Zones['Z'].append(hpav)
		
		Zones['Name'].append('PZ2_' + str(floor + 1))
		Zones['X'].append(0)
		Zones['Y'].append(0)
		Zones['Z'].append(hpav)
		
		Zones['Name'].append('PZ3_' + str(floor + 1))
		Zones['X'].append(building_xlen)
		Zones['Y'].append(0)
		Zones['Z'].append(hpav)
		
		Zones['Name'].append('PZ4_' + str(floor + 1))
		Zones['X'].append(building_xlen)
		Zones['Y'].append(building_ylen)
		Zones['Z'].append(hpav)
		
		Zones['Name'].append('CORE_' + str(floor + 1))
		Zones['X'].append(shell_depth)
		Zones['Y'].append(shell_depth)
		Zones['Z'].append(hpav)

		#### PZ (Perimetral Zones) ####
		
		# PZ1 -------------------------------------------
		
		#Order Always: Floor, Ceiling, 1, 2, 3, 4
		BldgSurface['Name'].append('Floor_'+Zones['Name'][-5])
		BldgSurface['Name'].append('Ceiling_'+Zones['Name'][-5])
		BldgSurface['Name'].append('Wall1_'+Zones['Name'][-5])
		BldgSurface['Name'].append('Wall2_'+Zones['Name'][-5])
		BldgSurface['Name'].append('Wall3_'+Zones['Name'][-5])
		BldgSurface['Name'].append('Wall4_'+Zones['Name'][-5])
		
		BldgSurface['SurfaceType'].append('Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['SurfaceType'].append('Roof')
		else:
			BldgSurface['SurfaceType'].append('Ceiling')
			
		for j in range(4):
			BldgSurface['SurfaceType'].append('Wall')
		
		# Se for o mesmo construction, nao precisa

		if boundaries == 'bot' or boundaries == 'exp' or boundaries == 'pex' or boundaries == 'pad':
			BldgSurface['ConstructionName'].append(floor_construction)
		else:
			BldgSurface['ConstructionName'].append('Interior Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['ConstructionName'].append(roof_construction)
		else:
			BldgSurface['ConstructionName'].append('InteriorCeiling')

		for j in range(6):
			BldgSurface['ZoneName'].append(Zones['Name'][-5])

		BldgSurface['ConstructionName'].append(extwall_construction)
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		
		# PZ2 -------------------------------------------
		
		BldgSurface['Name'].append('Floor_'+Zones['Name'][-4])
		BldgSurface['Name'].append('Ceiling_'+Zones['Name'][-4])
		BldgSurface['Name'].append('Wall1_'+Zones['Name'][-4])
		BldgSurface['Name'].append('Wall2_'+Zones['Name'][-4])
		BldgSurface['Name'].append('Wall3_'+Zones['Name'][-4])
		BldgSurface['Name'].append('Wall4_'+Zones['Name'][-4])
		
		BldgSurface['SurfaceType'].append('Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['SurfaceType'].append('Roof')
		else:
			BldgSurface['SurfaceType'].append('Ceiling')
			
		for j in range(4):
			BldgSurface['SurfaceType'].append('Wall')
		
		# Se for o mesmo construction, nao precisa

		if boundaries == 'bot' or boundaries == 'exp' or boundaries == 'pex' or boundaries == 'pad':
			BldgSurface['ConstructionName'].append(floor_construction)
		else:
			BldgSurface['ConstructionName'].append('Interior Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['ConstructionName'].append(roof_construction)
		else:
			BldgSurface['ConstructionName'].append('InteriorCeiling')

		for j in range(6):
			BldgSurface['ZoneName'].append(Zones['Name'][-4])

		BldgSurface['ConstructionName'].append(extwall_construction)
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		
		# PZ3 -------------------------------------------
		
		BldgSurface['Name'].append('Floor_'+Zones['Name'][-3])
		BldgSurface['Name'].append('Ceiling_'+Zones['Name'][-3])
		BldgSurface['Name'].append('Wall1_'+Zones['Name'][-3])
		BldgSurface['Name'].append('Wall2_'+Zones['Name'][-3])
		BldgSurface['Name'].append('Wall3_'+Zones['Name'][-3])
		BldgSurface['Name'].append('Wall4_'+Zones['Name'][-3])
		
		BldgSurface['SurfaceType'].append('Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['SurfaceType'].append('Roof')
		else:
			BldgSurface['SurfaceType'].append('Ceiling')
			
		for j in range(4):
			BldgSurface['SurfaceType'].append('Wall')
		
		# Se for o mesmo construction, nao precisa

		if boundaries == 'bot' or boundaries == 'exp' or boundaries == 'pex' or boundaries == 'pad':
			BldgSurface['ConstructionName'].append(floor_construction)
		else:
			BldgSurface['ConstructionName'].append('Interior Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['ConstructionName'].append(roof_construction)
		else:
			BldgSurface['ConstructionName'].append('InteriorCeiling')

		for j in range(6):
			BldgSurface['ZoneName'].append(Zones['Name'][-3])

		BldgSurface['ConstructionName'].append(extwall_construction)
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')

		
		# PZ4 -------------------------------------------
		
		BldgSurface['Name'].append('Floor_'+Zones['Name'][-2])
		BldgSurface['Name'].append('Ceiling_'+Zones['Name'][-2])
		BldgSurface['Name'].append('Wall1_'+Zones['Name'][-2])
		BldgSurface['Name'].append('Wall2_'+Zones['Name'][-2])
		BldgSurface['Name'].append('Wall3_'+Zones['Name'][-2])
		BldgSurface['Name'].append('Wall4_'+Zones['Name'][-2])
		
		BldgSurface['SurfaceType'].append('Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['SurfaceType'].append('Roof')
		else:
			BldgSurface['SurfaceType'].append('Ceiling')
			
		for j in range(4):
			BldgSurface['SurfaceType'].append('Wall')
		
		# Se for o mesmo construction, nao precisa

		if boundaries == 'bot' or boundaries == 'exp' or boundaries == 'pex' or boundaries == 'pad':
			BldgSurface['ConstructionName'].append(floor_construction)
		else:
			BldgSurface['ConstructionName'].append('Interior Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['ConstructionName'].append(roof_construction)
		else:
			BldgSurface['ConstructionName'].append('InteriorCeiling')

		for j in range(6):
			BldgSurface['ZoneName'].append(Zones['Name'][-2])

		BldgSurface['ConstructionName'].append(extwall_construction)
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')

		# CORE --------------------------------------------------------
		
		BldgSurface['Name'].append('Floor_'+Zones['Name'][-1])
		BldgSurface['Name'].append('Ceiling_'+Zones['Name'][-1])
		BldgSurface['Name'].append('Wall1_'+Zones['Name'][-1])
		BldgSurface['Name'].append('Wall2_'+Zones['Name'][-1])
		BldgSurface['Name'].append('Wall3_'+Zones['Name'][-1])
		BldgSurface['Name'].append('Wall4_'+Zones['Name'][-1])
			
		BldgSurface['SurfaceType'].append('Floor')
		if boundaries == 'top' or boundaries == 'exp' or boundaries == 'pex':
			BldgSurface['SurfaceType'].append('Roof')
		else:
			BldgSurface['SurfaceType'].append('Ceiling')
			
		for j in range(4):
			BldgSurface['SurfaceType'].append('Wall')
		
		# Se for o mesmo construction, nao precisa
		if boundaries == 'bot' or boundaries == 'exp' or boundaries == 'pex' or boundaries == 'pad':
			BldgSurface['ConstructionName'].append(floor_construction)
		else:
			BldgSurface['ConstructionName'].append('Interior Floor')
		if boundaries == 'top' or boundaries == 'exp':
			BldgSurface['ConstructionName'].append(roof_construction)
		else:
			BldgSurface['ConstructionName'].append('InteriorCeiling')
		
		for j in range(6):
			BldgSurface['ZoneName'].append(Zones['Name'][-1])  

		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')
		BldgSurface['ConstructionName'].append('Parede Interna')

		#ALL ZONES... ------------------------------------------
		
		if boundaries == 'exp':
			floor_cond = 'Ground'
			ceiling_cond = 'Outdoors'

		elif boundaries == 'bot':
			floor_cond = 'Ground'
			if adiabatic_floor_model == 1:
				ceiling_cond = 'Adiabatic'
			else:
				ceiling_cond = 'Surface'
				upper_floor = str(floor+2)

		elif boundaries == 'mid':
			if adiabatic_floor_model == 1:
				floor_cond = 'Adiabatic'
				ceiling_cond = 'Adiabatic'
			else:
				floor_cond = 'Surface'
				lower_floor = str(floor)
				ceiling_cond = 'Surface'
				upper_floor = str(floor+2)

		elif boundaries == 'top':
			ceiling_cond = 'Outdoors'
			if adiabatic_floor_model == 1:
				floor_cond = 'Adiabatic'
			else:
				floor_cond = 'Surface'
				lower_floor = str(floor)            
		
		elif boundaries == 'pex':
			floor_cond = 'Outdoors'
			ceiling_cond = 'Outdoors'
		
		elif boundaries == 'pad':
			floor_cond = 'Outdoors'
			if adiabatic_floor_model == 1:
				ceiling_cond = 'Adiabatic'
			else:
				ceiling_cond = 'Surface'
				upper_floor = str(floor+2)
		
		else:
			print('The boundary name ' + str(boundaries) + ' is not valid... :)')
		
		BldgSurface['OutsideBoundaryCond'] += [floor_cond,ceiling_cond,'Outdoors','Surface','Surface','Surface',floor_cond,ceiling_cond,'Outdoors','Surface','Surface','Surface',floor_cond,ceiling_cond,'Outdoors','Surface','Surface','Surface',floor_cond,ceiling_cond,'Outdoors','Surface','Surface','Surface',floor_cond,ceiling_cond,'Surface','Surface','Surface','Surface']
		
		if adiabatic_floor_model == 1 or boundaries == 'exp' or boundaries == 'pex':
			BldgSurface['OutsideBoundryCondObj'] += [
			'','','','Wall4_PZ2_'+str(floor+1),'Wall1_CORE_'+str(floor+1),'Wall2_PZ4_'+str(floor+1),
			'','','','Wall4_PZ3_'+str(floor+1),'Wall2_CORE_'+str(floor+1),'Wall2_PZ1_'+str(floor+1),
			'','','','Wall4_PZ4_'+str(floor+1),'Wall3_CORE_'+str(floor+1),'Wall2_PZ2_'+str(floor+1),
			'','','','Wall4_PZ1_'+str(floor+1),'Wall4_CORE_'+str(floor+1),'Wall2_PZ3_'+str(floor+1),
			'','','Wall3_PZ1_'+str(floor+1),'Wall3_PZ2_'+str(floor+1),'Wall3_PZ3_'+str(floor+1),'Wall3_PZ4_'+str(floor+1)]
		else:
			if boundaries == 'bot' or boundaries == 'pad':
				BldgSurface['OutsideBoundryCondObj'] += [
				'','Floor_PZ1_'+upper_floor,'','Wall4_PZ2_'+str(floor+1),'Wall1_CORE_'+str(floor+1),'Wall2_PZ4_'+str(floor+1),
				'','Floor_PZ2_'+upper_floor,'','Wall4_PZ3_'+str(floor+1),'Wall2_CORE_'+str(floor+1),'Wall2_PZ1_'+str(floor+1),
				'','Floor_PZ3_'+upper_floor,'','Wall4_PZ4_'+str(floor+1),'Wall3_CORE_'+str(floor+1),'Wall2_PZ2_'+str(floor+1),
				'','Floor_PZ4_'+upper_floor,'','Wall4_PZ1_'+str(floor+1),'Wall4_CORE_'+str(floor+1),'Wall2_PZ3_'+str(floor+1),
				'','Floor_CORE_'+upper_floor,'Wall3_PZ1_'+str(floor+1),'Wall3_PZ2_'+str(floor+1),'Wall3_PZ3_'+str(floor+1),'Wall3_PZ4_'+str(floor+1)]
			elif boundaries == 'mid':
				BldgSurface['OutsideBoundryCondObj'] += [
				'Ceiling_PZ1_'+lower_floor,'Floor_PZ1_'+upper_floor,'','Wall4_PZ2_'+str(floor+1),'Wall1_CORE_'+str(floor+1),'Wall2_PZ4_'+str(floor+1),
				'Ceiling_PZ2_'+lower_floor,'Floor_PZ2_'+upper_floor,'','Wall4_PZ3_'+str(floor+1),'Wall2_CORE_'+str(floor+1),'Wall2_PZ1_'+str(floor+1),
				'Ceiling_PZ3_'+lower_floor,'Floor_PZ3_'+upper_floor,'','Wall4_PZ4_'+str(floor+1),'Wall3_CORE_'+str(floor+1),'Wall2_PZ2_'+str(floor+1),
				'Ceiling_PZ4_'+lower_floor,'Floor_PZ4_'+upper_floor,'','Wall4_PZ1_'+str(floor+1),'Wall4_CORE_'+str(floor+1),'Wall2_PZ3_'+str(floor+1),
				'Ceiling_CORE_'+lower_floor,'Floor_CORE_'+upper_floor,'Wall3_PZ1_'+str(floor+1),'Wall3_PZ2_'+str(floor+1),'Wall3_PZ3_'+str(floor+1),'Wall3_PZ4_'+str(floor+1)]
			elif boundaries == 'top':
				BldgSurface['OutsideBoundryCondObj'] += [
				'Ceiling_PZ1_'+lower_floor,'','','Wall4_PZ2_'+str(floor+1),'Wall1_CORE_'+str(floor+1),'Wall2_PZ4_'+str(floor+1),
				'Ceiling_PZ2_'+lower_floor,'','','Wall4_PZ3_'+str(floor+1),'Wall2_CORE_'+str(floor+1),'Wall2_PZ1_'+str(floor+1),
				'Ceiling_PZ3_'+lower_floor,'','','Wall4_PZ4_'+str(floor+1),'Wall3_CORE_'+str(floor+1),'Wall2_PZ2_'+str(floor+1),
				'Ceiling_PZ4_'+lower_floor,'','','Wall4_PZ1_'+str(floor+1),'Wall4_CORE_'+str(floor+1),'Wall2_PZ3_'+str(floor+1),
				'Ceiling_CORE_'+lower_floor,'','Wall3_PZ1_'+str(floor+1),'Wall3_PZ2_'+str(floor+1),'Wall3_PZ3_'+str(floor+1),'Wall3_PZ4_'+str(floor+1)]
		
		# Geometry -----------------------------------------------
		
		## PZ1

		#Floor
		BldgSurface['V1x'].append(building_xlen)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(0)
		BldgSurface['V2x'].append(building_xlen-shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(0)

		#Ceiling
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(ceiling_height)
		BldgSurface['V3x'].append(building_xlen-shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(ceiling_height)
		BldgSurface['V4x'].append(building_xlen)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall1
		BldgSurface['V1x'].append(building_xlen)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(building_xlen)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall2
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(shell_depth)
		BldgSurface['V4y'].append(-shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall3
		BldgSurface['V1x'].append(shell_depth)
		BldgSurface['V1y'].append(-shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(building_xlen-shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(building_xlen-shell_depth)
		BldgSurface['V4y'].append(-shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall4
		BldgSurface['V1x'].append(building_xlen-shell_depth)
		BldgSurface['V1y'].append(-shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(building_xlen-shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(building_xlen)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(building_xlen)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		## PZ2

		#Floor
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(building_ylen)
		BldgSurface['V1z'].append(0)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(building_ylen-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(0)

		#Ceiling
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(shell_depth)
		BldgSurface['V2z'].append(ceiling_height)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(building_ylen-shell_depth)
		BldgSurface['V3z'].append(ceiling_height)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(building_ylen)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall1
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(building_ylen)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(building_ylen)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall2
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(shell_depth)
		BldgSurface['V4y'].append(shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall3
		BldgSurface['V1x'].append(shell_depth)
		BldgSurface['V1y'].append(shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(shell_depth)
		BldgSurface['V3y'].append(building_ylen-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(shell_depth)
		BldgSurface['V4y'].append(building_ylen-shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall4
		BldgSurface['V1x'].append(shell_depth)
		BldgSurface['V1y'].append(building_ylen-shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(shell_depth)
		BldgSurface['V2y'].append(building_ylen-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(building_ylen)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(building_ylen)
		BldgSurface['V4z'].append(ceiling_height)
		
		## PZ3

		#Floor
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(0)
		BldgSurface['V2x'].append(-building_xlen)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-building_xlen+shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-shell_depth)
		BldgSurface['V4y'].append(shell_depth)
		BldgSurface['V4z'].append(0)

		#Ceiling
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(shell_depth)
		BldgSurface['V2z'].append(ceiling_height)
		BldgSurface['V3x'].append(-building_xlen+shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(ceiling_height)
		BldgSurface['V4x'].append(-building_xlen)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall1
		BldgSurface['V1x'].append(-building_xlen)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-building_xlen)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall2
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-shell_depth)
		BldgSurface['V4y'].append(shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall3
		BldgSurface['V1x'].append(-shell_depth)
		BldgSurface['V1y'].append(shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-building_xlen+shell_depth)
		BldgSurface['V3y'].append(shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-building_xlen+shell_depth)
		BldgSurface['V4y'].append(shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall4
		BldgSurface['V1x'].append(-building_xlen+shell_depth)
		BldgSurface['V1y'].append(shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-building_xlen+shell_depth)
		BldgSurface['V2y'].append(shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-building_xlen)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-building_xlen)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		## PZ4

		#Floor
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(-building_ylen)
		BldgSurface['V1z'].append(0)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(-building_ylen+shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(0)

		#Ceiling
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(ceiling_height)
		BldgSurface['V3x'].append(-shell_depth)
		BldgSurface['V3y'].append(-building_ylen+shell_depth)
		BldgSurface['V3z'].append(ceiling_height)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(-building_ylen)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall1
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(-building_ylen)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(-building_ylen)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall2
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-shell_depth)
		BldgSurface['V3y'].append(-shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-shell_depth)
		BldgSurface['V4y'].append(-shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall3
		BldgSurface['V1x'].append(-shell_depth)
		BldgSurface['V1y'].append(-shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(-shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(-shell_depth)
		BldgSurface['V3y'].append(-building_ylen+shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(-shell_depth)
		BldgSurface['V4y'].append(-building_ylen+shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall4
		BldgSurface['V1x'].append(-shell_depth)
		BldgSurface['V1y'].append(-building_ylen+shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(-shell_depth)
		BldgSurface['V2y'].append(-building_ylen+shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(-building_ylen)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(-building_ylen)
		BldgSurface['V4z'].append(ceiling_height)

		## CORE

		#Floor
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(0)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(building_ylen-2*shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(building_xlen-2*shell_depth)
		BldgSurface['V3y'].append(building_ylen-2*shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(building_xlen-2*shell_depth)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(0)

		#Ceiling
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(building_xlen-2*shell_depth)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(ceiling_height)
		BldgSurface['V3x'].append(building_xlen-2*shell_depth)
		BldgSurface['V3y'].append(building_ylen-2*shell_depth)
		BldgSurface['V3z'].append(ceiling_height)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(building_ylen-2*shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall1
		BldgSurface['V1x'].append(building_xlen-2*shell_depth)
		BldgSurface['V1y'].append(building_ylen-2*shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(building_xlen-2*shell_depth)
		BldgSurface['V2y'].append(building_ylen-2*shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(building_ylen-2*shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(building_ylen-2*shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall2
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(building_ylen-2*shell_depth)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(building_ylen-2*shell_depth)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(0)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(0)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall3
		BldgSurface['V1x'].append(0)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(0)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(building_xlen-2*shell_depth)
		BldgSurface['V3y'].append(0)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(building_xlen-2*shell_depth)
		BldgSurface['V4y'].append(0)
		BldgSurface['V4z'].append(ceiling_height)

		#Wall4
		BldgSurface['V1x'].append(building_xlen-2*shell_depth)
		BldgSurface['V1y'].append(0)
		BldgSurface['V1z'].append(ceiling_height)
		BldgSurface['V2x'].append(building_xlen-2*shell_depth)
		BldgSurface['V2y'].append(0)
		BldgSurface['V2z'].append(0)
		BldgSurface['V3x'].append(building_xlen-2*shell_depth)
		BldgSurface['V3y'].append(building_ylen-2*shell_depth)
		BldgSurface['V3z'].append(0)
		BldgSurface['V4x'].append(building_xlen-2*shell_depth)
		BldgSurface['V4y'].append(building_ylen-2*shell_depth)
		BldgSurface['V4z'].append(ceiling_height)

		##### FenestrationSurdace:Detailed ---------------------------------------------------------------
		
		#Janelas ocupam toda a largura da facada, no meio da parede. (distancia milimetrica para nao sobrepor arestas)
		## Create Windows
			
		# PZ1 Window
		
		FenSurface['Name'].append('Window_PZ1_'+str(floor+1))
		FenSurface['SurfaceType'].append('Window')
		FenSurface['ConstructionName'].append(glass_construction)
		FenSurface['BuildingSurfaceName'].append('Wall1_PZ1_'+str(floor+1))
		FenSurface['OutsideBoundryCondObj'].append('')
		FenSurface['ShadingControlName'].append('')
		FenSurface['V1x'].append(window_x2)
		FenSurface['V1y'].append(0)
		FenSurface['V1z'].append(window_z2)
		FenSurface['V2x'].append(window_x2)
		FenSurface['V2y'].append(0)
		FenSurface['V2z'].append(window_z1)
		FenSurface['V3x'].append(window_x1)
		FenSurface['V3y'].append(0)
		FenSurface['V3z'].append(window_z1)
		FenSurface['V4x'].append(window_x1)
		FenSurface['V4y'].append(0)
		FenSurface['V4z'].append(window_z2)

			
		# PZ2 Window
		
		FenSurface['Name'].append('Window_PZ2_'+str(floor+1))
		FenSurface['SurfaceType'].append('Window')
		FenSurface['ConstructionName'].append(glass_construction)
		FenSurface['BuildingSurfaceName'].append('Wall1_PZ2_'+str(floor+1))
		FenSurface['OutsideBoundryCondObj'].append('')
		FenSurface['ShadingControlName'].append('')
		FenSurface['V1x'].append(0)
		FenSurface['V1y'].append(window_y2)
		FenSurface['V1z'].append(window_z2)
		FenSurface['V2x'].append(0)
		FenSurface['V2y'].append(window_y2)
		FenSurface['V2z'].append(window_z1)
		FenSurface['V3x'].append(0)
		FenSurface['V3y'].append(window_y1)
		FenSurface['V3z'].append(window_z1)
		FenSurface['V4x'].append(0)
		FenSurface['V4y'].append(window_y1)
		FenSurface['V4z'].append(window_z2)
			
		# PZ3 Window
		
		FenSurface['Name'].append('Window_PZ3_'+str(floor+1))
		FenSurface['SurfaceType'].append('Window')
		FenSurface['ConstructionName'].append(glass_construction)
		FenSurface['BuildingSurfaceName'].append('Wall1_PZ3_'+str(floor+1))
		FenSurface['OutsideBoundryCondObj'].append('')
		FenSurface['ShadingControlName'].append('')
		FenSurface['V1x'].append(-window_x2)
		FenSurface['V1y'].append(0)
		FenSurface['V1z'].append(window_z2)
		FenSurface['V2x'].append(-window_x2)
		FenSurface['V2y'].append(0)
		FenSurface['V2z'].append(window_z1)
		FenSurface['V3x'].append(-window_x1)
		FenSurface['V3y'].append(0)
		FenSurface['V3z'].append(window_z1)
		FenSurface['V4x'].append(-window_x1)
		FenSurface['V4y'].append(0)
		FenSurface['V4z'].append(window_z2)

			
		# PZ4 Window
		
		FenSurface['Name'].append('Window_PZ4_'+str(floor+1))
		FenSurface['SurfaceType'].append('Window')
		FenSurface['ConstructionName'].append(glass_construction)
		FenSurface['BuildingSurfaceName'].append('Wall1_PZ4_'+str(floor+1))
		FenSurface['OutsideBoundryCondObj'].append('')
		FenSurface['ShadingControlName'].append('')
		FenSurface['V1x'].append(0)
		FenSurface['V1y'].append(-window_y2)
		FenSurface['V1z'].append(window_z2)
		FenSurface['V2x'].append(0)
		FenSurface['V2y'].append(-window_y2)
		FenSurface['V2z'].append(window_z1)
		FenSurface['V3x'].append(0)
		FenSurface['V3y'].append(-window_y1)
		FenSurface['V3z'].append(window_z1)
		FenSurface['V4x'].append(0)
		FenSurface['V4y'].append(-window_y1)
		FenSurface['V4z'].append(window_z2)

	   ##### Shading ----------------------------------------------------
	 
		if shading > 0:
	 
			shading_z_height = window_z2+hpav if str(shading_ceiling_height) != "1" else ceiling_height+hpav
	
			# Shading PZ1

			ShadingBuildingDetailed['Name'].append('Shading1_'+str(floor+1))
			ShadingBuildingDetailed['Transmittance Schedule Name'].append('')
			ShadingBuildingDetailed['Number of Vertices'].append(4)
			ShadingBuildingDetailed['V1x'].append(0) 
			ShadingBuildingDetailed['V1y'].append(building_ylen+shading)
			ShadingBuildingDetailed['V1z'].append(shading_z_height) 
			ShadingBuildingDetailed['V2x'].append(0)
			ShadingBuildingDetailed['V2y'].append(building_ylen)        
			ShadingBuildingDetailed['V2z'].append(shading_z_height) 	
			ShadingBuildingDetailed['V3x'].append(building_xlen)                    
			ShadingBuildingDetailed['V3y'].append(building_ylen)
			ShadingBuildingDetailed['V3z'].append(shading_z_height) 	
			ShadingBuildingDetailed['V4x'].append(building_xlen)
			ShadingBuildingDetailed['V4y'].append(building_ylen+shading)
			ShadingBuildingDetailed['V4z'].append(shading_z_height) 	
	 
			# Shading PZ2
			
			ShadingBuildingDetailed['Name'].append('Shading2_'+str(floor+1))
			ShadingBuildingDetailed['Transmittance Schedule Name'].append('')
			ShadingBuildingDetailed['Number of Vertices'].append(4)
			ShadingBuildingDetailed['V1x'].append(-shading) 
			ShadingBuildingDetailed['V1y'].append(0)
			ShadingBuildingDetailed['V1z'].append(shading_z_height) 
			ShadingBuildingDetailed['V2x'].append(0)
			ShadingBuildingDetailed['V2y'].append(0)        
			ShadingBuildingDetailed['V2z'].append(shading_z_height) 	
			ShadingBuildingDetailed['V3x'].append(0)                    
			ShadingBuildingDetailed['V3y'].append(building_ylen)
			ShadingBuildingDetailed['V3z'].append(shading_z_height) 
			ShadingBuildingDetailed['V4x'].append(-shading)
			ShadingBuildingDetailed['V4y'].append(building_ylen)
			ShadingBuildingDetailed['V4z'].append(shading_z_height) 

	 
			# Shading PZ3
			
			ShadingBuildingDetailed['Name'].append('Shading3_'+str(floor+1))
			ShadingBuildingDetailed['Transmittance Schedule Name'].append('')
			ShadingBuildingDetailed['Number of Vertices'].append(4)
			ShadingBuildingDetailed['V1x'].append(building_xlen) 
			ShadingBuildingDetailed['V1y'].append(-shading)
			ShadingBuildingDetailed['V1z'].append(shading_z_height) 
			ShadingBuildingDetailed['V2x'].append(building_xlen)
			ShadingBuildingDetailed['V2y'].append(0)        
			ShadingBuildingDetailed['V2z'].append(shading_z_height)
			ShadingBuildingDetailed['V3x'].append(0)                    
			ShadingBuildingDetailed['V3y'].append(0)
			ShadingBuildingDetailed['V3z'].append(shading_z_height) 
			ShadingBuildingDetailed['V4x'].append(0)
			ShadingBuildingDetailed['V4y'].append(-shading)
			ShadingBuildingDetailed['V4z'].append(shading_z_height)
	 
			# Shading PZ4
			
			ShadingBuildingDetailed['Name'].append('Shading4_'+str(floor+1))
			ShadingBuildingDetailed['Transmittance Schedule Name'].append('')
			ShadingBuildingDetailed['Number of Vertices'].append(4)
			ShadingBuildingDetailed['V1x'].append(building_xlen+shading) 
			ShadingBuildingDetailed['V1y'].append(building_ylen)
			ShadingBuildingDetailed['V1z'].append(shading_z_height) 
			ShadingBuildingDetailed['V2x'].append(building_xlen)
			ShadingBuildingDetailed['V2y'].append(building_ylen)        
			ShadingBuildingDetailed['V2z'].append(shading_z_height)
			ShadingBuildingDetailed['V3x'].append(building_xlen)                    
			ShadingBuildingDetailed['V3y'].append(0)
			ShadingBuildingDetailed['V3z'].append(shading_z_height) 
			ShadingBuildingDetailed['V4x'].append(building_xlen+shading)
			ShadingBuildingDetailed['V4y'].append(0)
			ShadingBuildingDetailed['V4z'].append(shading_z_height)

	for i in range(len(BldgSurface['OutsideBoundaryCond'])): 
	
		if boundaries != 'pex' or boundaries != 'pad':
			if BldgSurface['OutsideBoundaryCond'][i] == 'Outdoors':
				BldgSurface['SunExposure'].append('SunExposed')
				BldgSurface['WindExposure'].append('WindExposed')
			else:
				BldgSurface['SunExposure'].append('NoSun')
				BldgSurface['WindExposure'].append('NoWind')
		else:
			if BldgSurface['SurfaceType'][i] == 'Floor':
				BldgSurface['SunExposure'].append('NoSun')
				BldgSurface['WindExposure'].append('WindExposed')
			else:
				if BldgSurface['OutsideBoundaryCond'][i] == 'Outdoors':
					BldgSurface['SunExposure'].append('SunExposed')
					BldgSurface['WindExposure'].append('WindExposed')
				else:
					BldgSurface['SunExposure'].append('NoSun')
					BldgSurface['WindExposure'].append('NoWind')
	   
	####################------ ADD TO IDF ------####################
	
	# BuildingSurface:Detailed
	
	for i in range(len(BldgSurface['Name'])):
		obj = IDF._create_datadict("BuildingSurface:Detailed")
		obj['Name'] = BldgSurface['Name'][i]
		obj['Surface Type'] = BldgSurface['SurfaceType'][i]
		obj['Construction Name'] = BldgSurface['ConstructionName'][i]
		obj['Zone Name'] = BldgSurface['ZoneName'][i]
		obj['Outside Boundary Condition'] = BldgSurface['OutsideBoundaryCond'][i]
		obj['Outside Boundary Condition Object'] = BldgSurface['OutsideBoundryCondObj'][i]
		obj['Sun Exposure'] = BldgSurface['SunExposure'][i]
		obj['Wind Exposure'] = BldgSurface['WindExposure'][i]
		obj['Number of Vertices'] = 4
		obj[u'Vertex 1 X-coordinate', 0] = BldgSurface['V1x'][i]
		obj[u'Vertex 1 Y-coordinate', 0] = BldgSurface['V1y'][i]
		obj[u'Vertex 1 Z-coordinate', 0] = BldgSurface['V1z'][i]
		obj[u'Vertex 1 X-coordinate', 1] = BldgSurface['V2x'][i]
		obj[u'Vertex 1 Y-coordinate', 1] = BldgSurface['V2y'][i]
		obj[u'Vertex 1 Z-coordinate', 1] = BldgSurface['V2z'][i]
		obj[u'Vertex 1 X-coordinate', 2] = BldgSurface['V3x'][i]
		obj[u'Vertex 1 Y-coordinate', 2] = BldgSurface['V3y'][i]
		obj[u'Vertex 1 Z-coordinate', 2] = BldgSurface['V3z'][i]
		obj[u'Vertex 1 X-coordinate', 3] = BldgSurface['V4x'][i]
		obj[u'Vertex 1 Y-coordinate', 3] = BldgSurface['V4y'][i]
		obj[u'Vertex 1 Z-coordinate', 3] = BldgSurface['V4z'][i]  
		idf.add(obj)

	#FenestrationSurface:Detailed
	
	for i in range(len(FenSurface['Name'])):
		obj = IDF._create_datadict("FenestrationSurface:Detailed")
		obj["Name"] = FenSurface['Name'][i]
		obj["Surface Type"] = FenSurface['SurfaceType'][i]
		obj["Construction Name"] = FenSurface["ConstructionName"][i]
		obj["Building Surface Name"] = FenSurface['BuildingSurfaceName'][i]
		obj["Outside Boundary Condition Object"] = FenSurface['OutsideBoundryCondObj'][i]
		obj["Shading Control Name"] = FenSurface['ShadingControlName'][i]
		obj['Number of Vertices'] = 4
		obj['Vertex 1 X-coordinate'] = FenSurface['V1x'][i]
		obj['Vertex 1 Y-coordinate'] = FenSurface['V1y'][i]
		obj['Vertex 1 Z-coordinate'] = FenSurface['V1z'][i]
		obj['Vertex 2 X-coordinate'] = FenSurface['V2x'][i]
		obj['Vertex 2 Y-coordinate'] = FenSurface['V2y'][i]
		obj['Vertex 2 Z-coordinate'] = FenSurface['V2z'][i]
		obj['Vertex 3 X-coordinate'] = FenSurface['V3x'][i]
		obj['Vertex 3 Y-coordinate'] = FenSurface['V3y'][i]
		obj['Vertex 3 Z-coordinate'] = FenSurface['V3z'][i]
		obj['Vertex 4 X-coordinate'] = FenSurface['V4x'][i]
		obj['Vertex 4 Y-coordinate'] = FenSurface['V4y'][i]
		obj['Vertex 4 Z-coordinate'] = FenSurface['V4z'][i] 
		idf.add(obj)
	
	# Shading:Building:Detailed
	if shading > 0:  
		for i in range(len(ShadingBuildingDetailed['Name'])):
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj['Name'] = ShadingBuildingDetailed['Name'][i]
			obj['Transmittance Schedule Name'] = ShadingBuildingDetailed['Transmittance Schedule Name'][i]
			obj['Number of Vertices'] = 4
			obj[u'Vertex 1 X-coordinate', 0] = ShadingBuildingDetailed['V1x'][i]
			obj[u'Vertex 1 Y-coordinate', 0] = ShadingBuildingDetailed['V1y'][i]
			obj[u'Vertex 1 Z-coordinate', 0] = ShadingBuildingDetailed['V1z'][i]
			obj[u'Vertex 1 X-coordinate', 1] = ShadingBuildingDetailed['V2x'][i]
			obj[u'Vertex 1 Y-coordinate', 1] = ShadingBuildingDetailed['V2y'][i]
			obj[u'Vertex 1 Z-coordinate', 1] = ShadingBuildingDetailed['V2z'][i]
			obj[u'Vertex 1 X-coordinate', 2] = ShadingBuildingDetailed['V3x'][i]
			obj[u'Vertex 1 Y-coordinate', 2] = ShadingBuildingDetailed['V3y'][i]
			obj[u'Vertex 1 Z-coordinate', 2] = ShadingBuildingDetailed['V3z'][i]
			obj[u'Vertex 1 X-coordinate', 3] = ShadingBuildingDetailed['V4x'][i]
			obj[u'Vertex 1 Y-coordinate', 3] = ShadingBuildingDetailed['V4y'][i]
			obj[u'Vertex 1 Z-coordinate', 3] = ShadingBuildingDetailed['V4z'][i]
			idf.add(obj)

	# Zones
	
	for i in range(len(Zones['Name'])):
		obj = IDF._create_datadict('Zone')
		obj['Name'] = Zones['Name'][i]
		obj['Direction of Relative North'] = 0
		obj['Multiplier'] = 1
		obj['X Origin'] = Zones['X'][i]
		obj['Y Origin'] = Zones['Y'][i]
		obj['Z Origin'] = Zones['Z'][i] 
		idf.add(obj)

	# Building
	obj = IDF._create_datadict('Building')
	obj['Name'] = output #(str(ceiling_height)+'-'+str(building_xlen)+'-'+str(building_ratio)+'-'+str(wwr)+'-'+str(boundaries))
	
	obj['North Axis'] = azimuth
	obj['Terrain'] = 'City'
	obj['Solar Distribution'] = 'FullInteriorAndExteriorWithReflections'
	obj['Loads Convergence Tolerance Value'] = 0.04
	obj['Temperature Convergence Tolerance Value'] = 0.4
	idf.add(obj)

	# People
	idf["People"][0][2]	= schedule
	idf["People"][0][5] = people
	
	# Lights
	idf["Lights"][0][2]	= schedule
	idf["Lights"][0][5] = lights

	# ElectricEquipment
	idf["ElectricEquipment"][0][2]	= schedule
	idf["ElectricEquipment"][0][5] = equip
	
	# HVACTemplate:Zone:IdealLoadsAirSystem
	for i in range(len(Zones['Name'])):
		obj = IDF._create_datadict('HVACTemplate:Zone:IdealLoadsAirSystem')
		obj['Zone Name'] = Zones['Name'][i]
		obj['Template Thermostat Name'] = 'Termostato'
		obj['System Availability Schedule Name'] = schedule
		obj['Maximum Heating Supply Air Temperature'] = 50.0
		obj['Minimum Cooling Supply Air Temperature'] = 13.0
		obj['Maximum Heating Supply Air Humidity Ratio'] = 0.0156
		obj['Minimum Cooling Supply Air Humidity Ratio'] = 0.0077
		obj['Heating Limit'] = 'NoLimit'
		obj['Maximum Heating Air Flow Rate'] = ''
		obj['Maximum Sensible Heating Capacity'] = ''
		obj['Cooling Limit'] = 'NoLimit'
		obj['Maximum Cooling Air Flow Rate'] = ''
		obj['Maximum Total Cooling Capacity'] = ''
		obj['Heating Availability Schedule Name'] = ''
		obj['Cooling Availability Schedule Name'] = ''
		obj['Dehumidification Control Type'] = 'None'
		obj['Cooling Sensible Heat Ratio'] = ''
		obj['Dehumidification Setpoint'] = ''
		obj['Humidification Control Type'] = 'None'
		obj['Humidification Setpoint'] = ''
		obj['Outdoor Air Method'] = 'Flow/Person'
		obj['Outdoor Air Flow Rate per Person'] = 0.0075
		obj['Outdoor Air Flow Rate per Zone Floor Area'] = ''
		obj['Outdoor Air Flow Rate per Zone'] = ''
		obj['Design Specification Outdoor Air Object Name'] = ''
		obj['Demand Controlled Ventilation Type'] = 'None'
		obj['Outdoor Air Economizer Type'] = 'NoEconomizer'
		obj['Heat Recovery Type'] = 'None'
		obj['Sensible Heat Recovery Effectiveness'] = ''
		obj['Latent Heat Recovery Effectiveness'] = ''
		idf.add(obj)

	obj = IDF._create_datadict('ZoneList')
	obj.name = "All"
	for i in range(len(Zones['Name'])):
		obj.add_extensible(Zones['Name'][i])
	idf.add(obj)

	for i in range(len(idf["Construction"])):
		if roof_construction == idf["Construction"][i]["Name"]:
			for j in range(len(idf["Material"])):
				if idf["Construction"][i][1] == idf["Material"][j]["Name"]:
					idf["Material"][j][-1] = abs_roof_construction
					idf["Material"][j][-2] = abs_roof_construction
		if extwall_construction == idf["Construction"][i]["Name"]:
			for j in range(len(idf["Material"])):
				if idf["Construction"][i][1] == idf["Material"][j]["Name"]:
					idf["Material"][j][-1] = abs_extwall_construction
					idf["Material"][j][-2] = abs_extwall_construction	

	for i in range(len(idf["FenestrationSurface:Detailed"])):
		obj = IDF._create_datadict('WindowProperty:ShadingControl')
		obj["Name"] = "Shading_"+idf["FenestrationSurface:Detailed"][i]["Name"][idf["FenestrationSurface:Detailed"][i]["Name"].find("PZ")+2:]
		obj[1] = "InteriorBlind"
		obj[2] = "" 
		obj[3] = "AlwaysOff"
		obj[4] = ""
		obj[5] = ""
		obj[6] = "No"
		obj[7] = "No"
		obj[8] = "PVC"
		obj[9] = "FixedSlatAngle"
		obj[10] = ""
		idf.add(obj)
		idf["FenestrationSurface:Detailed"][i][6] = obj["Name"]

	sur_length_x = building_xlen
	sur_length_y = building_ylen
	sur_heigth = sur_nfloor*ceiling_height
	built_area = 5*sur_length_x*sur_length_y
	urban_area = built_area/horizontal_building_density
	edge_x = sur_length_x/(horizontal_building_density**.5)
	edge_y = sur_length_y/(horizontal_building_density**.5)
	urban_length_x = 5*edge_x
	urban_length_y = 5*edge_y

	if building_block_position == 0:

		origin_x = -(2*(edge_x - sur_length_x) + 2*sur_length_x)
		origin_y = -(2*(edge_y - sur_length_y) + 2*sur_length_y)

		step_x = sur_length_x+(edge_x-sur_length_x)
		step_y = sur_length_y+(edge_y-sur_length_y)

		x_positions = [origin_x+i*step_x for i in range(5)]
		y_positions = [origin_y+i*step_y for i in range(5)]
		y_positions.reverse()

		sur_position = []
		for i in xrange(len(y_positions)):
			for j in xrange(len(x_positions)):
				sur_position.append([x_positions[j],y_positions[i]])
	
	elif building_block_position == 1:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]
	
	elif building_block_position == 12:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 2:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 23:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 3:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 34:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 4:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	elif building_block_position == 41:
		sur_position = [[-200,300],[-100,300],[0,300],[100,300],[200,300],
						[-200,150],[-100,150],[0,150],[100,150],[200,150],
						[-200,0],[-100,0],["pass","pass"],[100,0],[200,0],
						[-200,-150],[-100,-150],[0,-150],[100,-150],[200,-150],
						[-200,-300],[-100,-300],[0,-300],[100,-300],[200,-300]]

	for i in range(25):
		if i != 12:
			# Facing north
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj["Name"] = "sur_north_"+str(i)
			obj[1] = "Surroundings_visual_transmittance"
			obj[2] = "autocalculate"
			[obj[3], obj[4], obj[5]] = [sur_length_x+sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			[obj[6], obj[7], obj[8]] = [sur_length_x+sur_position[i][0], sur_length_y+sur_position[i][1], 0]
			[obj[9], obj[10], obj[11]] = [sur_position[i][0], sur_length_y+sur_position[i][1], 0]
			[obj[12], obj[13], obj[14]] = [sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			idf.add(obj)

			# Facing south
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj["Name"] = "sur_south_"+str(i)
			obj[1] = "Surroundings_visual_transmittance"
			obj[2] = "autocalculate"
			[obj[3], obj[4], obj[5]] = [sur_position[i][0], sur_position[i][1], sur_heigth]
			[obj[6], obj[7], obj[8]] = [sur_position[i][0] , sur_position[i][1], 0]
			[obj[9], obj[10], obj[11]] = [sur_length_x+sur_position[i][0], sur_position[i][1], 0]
			[obj[12], obj[13], obj[14]] = [sur_length_x+sur_position[i][0], sur_position[i][1], sur_heigth]
			idf.add(obj)

			# Facing east
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj["Name"] = "sur_east_"+str(i)
			obj[1] = "Surroundings_visual_transmittance"
			obj[2] = "autocalculate"
			[obj[3], obj[4], obj[5]] = [sur_length_x+sur_position[i][0], sur_position[i][1], sur_heigth]
			[obj[6], obj[7], obj[8]] = [sur_length_x+sur_position[i][0] , sur_position[i][1], 0]
			[obj[9], obj[10], obj[11]] = [sur_length_x+sur_position[i][0], sur_length_y+sur_position[i][1], 0]
			[obj[12], obj[13], obj[14]] = [sur_length_x+sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			idf.add(obj)

			# Facing west
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj["Name"] = "sur_west_"+str(i)
			obj[1] = "Surroundings_visual_transmittance"
			obj[2] = "autocalculate"
			[obj[3], obj[4], obj[5]] = [sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			[obj[6], obj[7], obj[8]] = [sur_position[i][0], sur_length_y+sur_position[i][1], 0]
			[obj[9], obj[10], obj[11]] = [sur_position[i][0], sur_position[i][1], 0]
			[obj[12], obj[13], obj[14]] = [sur_position[i][0], sur_position[i][1], sur_heigth]
			idf.add(obj) 	    

			# Facing sky
			obj = IDF._create_datadict('Shading:Building:Detailed')
			obj["Name"] = "sur_sky_"+str(i)
			obj[1] = "Surroundings_visual_transmittance"
			obj[2] = "autocalculate"
			[obj[3], obj[4], obj[5]] = [sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			[obj[6], obj[7], obj[8]] = [sur_position[i][0], sur_position[i][1], sur_heigth]
			[obj[9], obj[10], obj[11]] = [sur_length_x+sur_position[i][0], sur_position[i][1], sur_heigth]
			[obj[12], obj[13], obj[14]] = [sur_length_x+sur_position[i][0], sur_length_y+sur_position[i][1], sur_heigth]
			idf.add(obj)

	for i in range(len(idf['Shading:Building:Detailed'])):
		if "sky" not in idf['Shading:Building:Detailed'][i][0]:
			obj = IDF._create_datadict('ShadingProperty:Reflectance')
			obj[0] = idf['Shading:Building:Detailed'][i][0]
			obj[1] = sur_abs_extwall
			obj[2] = sur_abs_extwall
			obj[3] = sur_wwr
			obj[4] = sur_glass
			idf.add(obj)

	# Surroundings
	idf["Schedule:Compact"][0][5] = sur_vis_transmittance
	
	# Ideal Loads Thermostat
	idf["HVACTemplate:Thermostat"][0][4] = thermostat


	out = [idf, output]

	return(out)
	
def idfFunc(baseFile):
	idf = open(baseFile, 'r')
	return idf.read()
	
def sampleGen(file = 'sample idf Sobol.csv', path=os.getcwd()): #insert argument sys.argv here too, to change the sampling method, if needed!!!
	
	os.chdir(path)
	condition = False
	i = 0

	dadoSample = pd.read_csv(file)

	#incremental idf creation - just creates the idfs of certain iteration!
	n_iter = sys.argv[1]
	dadoSample = dadoSample[dadoSample["iteration_vector"] == int(n_iter)]
	dadoSample = dadoSample.reset_index()

	while i <= len(dadoSample):
		if condition == True:

			# Entradas da amostra.csv
			shell_depth = dadoSample["shell_depth"][i-1]
			ceiling_height = dadoSample["ceiling_height"][i-1]
			building_xlen = dadoSample["building_xlen"][i-1]
			building_ratio = dadoSample["building_ratio"][i-1]
			wwr = dadoSample["wwr"][i-1]
			shading = dadoSample["shading"][i-1]
			azimuth = dadoSample["azimuth"][i-1]
			nfloor = dadoSample["nfloor"][i-1]
			pilotis = dadoSample["pilotis"][i-1]
			
			# DEPOIS!
			floor_construction = dadoSample["floor_construction"][i-1]
			roof_construction = dadoSample["roof_construction"][i-1] 
			extwall_construction = dadoSample["extwall_construction"][i-1] 
			glass_construction = dadoSample["glass_construction"][i-1]
			schedule = dadoSample["schedule"][i-1]            
			people = dadoSample["people"][i-1]
			lights = dadoSample["lights"][i-1]
			equip = dadoSample["equip"][i-1]
			abs_roof_construction = dadoSample["abs_roof_construction"][i-1]
			abs_extwall_construction = dadoSample["abs_extwall_construction"][i-1]
			shading_ceiling_height = dadoSample["shading_ceiling_height"][i-1]

 			sur_nfloor = dadoSample["sur_nfloor"][i-1]
 			sur_abs_extwall = dadoSample["sur_abs_extwall"][i-1]
 			sur_wwr = dadoSample["sur_wwr"][i-1]
 			horizontal_building_density = dadoSample["horizontal_building_density"][i-1]
 			building_block_position = dadoSample["building_block_position"][i-1]
 			sur_glass = dadoSample["sur_glass"][i-1]
			sur_vis_transmittance = dadoSample["sur_vis_transmittance"][i-1]
			thermostat = dadoSample["thermostat"][i-1]


			caso = '{:06.0f}'.format(i)
			output = ('caso_{}.idf'.format(caso))
			print(output)
			#print(shell_depth, ceiling_height,building_xlen, building_ratio, wwr, shading, azimuth, nfloor, pilotis, output , path)
			out = main(shell_depth = shell_depth, 
				ceiling_height = ceiling_height, 
				building_xlen = building_xlen, 
				building_ratio = building_ratio, 
				wwr = wwr, 
				shading = shading, 
				azimuth = azimuth, 
				nfloor = nfloor, 
				pilotis = pilotis, 
				floor_construction = floor_construction, 
				roof_construction = roof_construction, 
				extwall_construction = extwall_construction, 
				glass_construction = glass_construction, 
				schedule = schedule, 
				people = people, 
				lights = lights, 
				equip = equip, 
				abs_roof_construction = abs_roof_construction, 
				abs_extwall_construction = abs_extwall_construction, 
				shading_ceiling_height = shading_ceiling_height, 
				sur_nfloor = sur_nfloor, 
				sur_abs_extwall = sur_abs_extwall, 
				sur_glass = sur_glass, 
				sur_wwr = sur_wwr, 
				horizontal_building_density = horizontal_building_density, 
				building_block_position = building_block_position, 
				sur_vis_transmittance = sur_vis_transmittance, 
				thermostat = thermostat, 
				output = output, 
				path = path)
					  
			idf = out[0]
			outputname = out[1]
			idf.save(outputname)
			
			#baseFile = outputname
			idfBase = idfFunc(outputname)
			
			#arquivo contendo a amostragem gerada pelo e++
			#dadoSample = pd.read_csv(file)

			# lista contendo strings com o nome dos parametros amostrados
			variaveis = dadoSample.keys()
			#print(variaveis)
			'''
			for j in variaveis:
				# para cada parametro amostrado vai procurar o nome da variavel no idfBase e substituir
				if "@@" in j:
					idfBase = idfBase.replace(j, str(dadoSample[j][i-1]))
				# eu nao testei o ## nesse codigo mas acredito que funcione... Aqui ele substitui o conteudo do arquivo que a variavel indica
				if "!##" in j:
				
					with open(dadoSample[j][i-1],'r') as file:              
						conteudo = file.read()
					idfBase = idfBase.replace(j, conteudo)
			# aqui vai escrever num idf a string que foi modificada na linha anterior tendo o nome da linha do sample.csv correspondente
			with open(outputname, 'w') as file:
				file.write(idfBase)
			'''
		condition = True
		i += 1

sampleGen(path=os.getcwd())