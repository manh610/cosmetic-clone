package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.dto.request.address.AddressSupplierRequest;
import com.cosmetic.gg.dto.response.address.AddressObjectResponse;
import com.cosmetic.gg.entity.address.Address;
import com.cosmetic.gg.entity.address.District;
import com.cosmetic.gg.entity.address.Province;
import com.cosmetic.gg.entity.address.Ward;
import com.cosmetic.gg.model.address.AddressModel;
import com.cosmetic.gg.service.address.AddressService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/address")
@AllArgsConstructor
@CrossOrigin
public class AddressController {

	private final AddressService addressService;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "code", required = false, defaultValue = "") String code,
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status,
			@RequestParam(value = "provinceId", required = false, defaultValue = "") String provinceId,
			@RequestParam(value = "districtId", required = false, defaultValue = "") String districtId,
			@RequestParam(value = "wardId", required = false, defaultValue = "") String wardId) {
		String transId = UUID.randomUUID().toString();
		try {
			List<Address> results = addressService.search(code, status, provinceId, districtId, wardId);
			return new ResponseEntity<>(
			        new Response<List<Address>>().success(
			          transId,
			          results.size(),
			          results
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "/user")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addUser(
			@RequestBody @Valid AddressModel addressModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			addressModel.setId("");
			Address address = ModelMapper.map(addressModel, Address.class);
			List<Error> errors = addressService.validator(address);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Address addressEntity = addressService.addUser(addressModel);
			if (Objects.isNull(addressEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Address>().success(
						transId,
						addressEntity
						),
				HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "/supplier")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addSupplier(
			@RequestBody @Valid AddressSupplierRequest addressSupplier,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			addressSupplier.setId("");
			Address address = ModelMapper.map(addressSupplier, Address.class);
			List<Error> errors = addressService.validator(address);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Address addressEntity = addressService.addSupplier(addressSupplier);
			if (Objects.isNull(addressEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Address>().success(
						transId,
						addressEntity
						),
				HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/user/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> updateUser(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid AddressModel addressModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			addressModel.setId(id);
			Address address = ModelMapper.map(addressModel, Address.class);
			List<Error> errors = addressService.validator(address);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			Address addressEntity = addressService.updateUser(addressModel);
			if (Objects.isNull(addressEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Address>().success(
							transId,
							addressEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/supplier/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> updateSupplier(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid AddressSupplierRequest addressSupplier,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			addressSupplier.setId(id);
			Address address = ModelMapper.map(addressSupplier, Address.class);
			List<Error> errors = addressService.validator(address);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			Address addressEntity = addressService.updateSupplier(addressSupplier);
			if (Objects.isNull(addressEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Address>().success(
							transId,
							addressEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@DeleteMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> delete(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = addressService.delete(id);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> detail(
		    @PathVariable(value = "id") String id,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			AddressModel address;
			try {
				address = addressService.detail(id);
			}catch(Exception ex) {
				address = null;
			}
			
			if (Objects.isNull(address))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<AddressModel>().success(
			          transId,
			          address
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/user/{userId}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getByUser(
			@PathVariable(value = "userId") String userId,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<AddressObjectResponse> results = addressService.getByUser(userId);
			return new ResponseEntity<>(
			        new Response<List<AddressObjectResponse>>().success(
			          transId,
			          results.size(),
			          results
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/supplier/{supplierId}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getBySupplier(
			@PathVariable(value = "supplierId") String supplierId,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<AddressObjectResponse> results = addressService.getBySupplier(supplierId);
			if(results.isEmpty())
				return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
			
			return new ResponseEntity<>(
			        new Response<List<AddressObjectResponse>>().success(
			          transId,
			          results.size(),
			          results
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/{userId}/{addressId}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> changeDefault(
			@PathVariable(value = "userId") String userId,
			@PathVariable(value = "addressId") String addressId,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = addressService.changeDefault(addressId, userId);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "provinces")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> provinces(
			@RequestParam(value = "name", required = false) String name,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<Province> provinceEntity = addressService.getProvinces(name);
			return new ResponseEntity<>(
			        new Response<List<Province>>().success(
			          transId,
			          provinceEntity.size(),
			          provinceEntity
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "districts")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> districts(
			@RequestParam(value = "provinceId", required = true) String provinceId,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<District> districtEntity = addressService.getDistricts(provinceId);
			return new ResponseEntity<>(
			        new Response<List<District>>().success(
			          transId,
			          districtEntity.size(),
			          districtEntity
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "wards")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> wards(
			@RequestParam(value = "districtId", required = true) String districtId,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<Ward> wardEntity = addressService.getWards(districtId);
			return new ResponseEntity<>(
			        new Response<List<Ward>>().success(
			          transId,
			          wardEntity.size(),
			          wardEntity
			        ),
			        HttpStatus.OK
			      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
