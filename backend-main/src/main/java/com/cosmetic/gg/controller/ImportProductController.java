package com.cosmetic.gg.controller;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
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
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.datetime.DateUtils;
import com.cosmetic.gg.dto.response.order.orderDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.dto.response.supply.ImportDetailResponse;
import com.cosmetic.gg.entity.supply.Import;
import com.cosmetic.gg.model.order.OrderModel;
import com.cosmetic.gg.model.supply.ImportModel;
import com.cosmetic.gg.service.supply.ImportService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/import-product")
@AllArgsConstructor
@CrossOrigin
public class ImportProductController {

	private final ImportService importService;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "supplierId", required = false, defaultValue = "") String supplierId,
			@RequestParam(value = "importDateFrom", required = false, defaultValue = "") String importDateFrom,
		    @RequestParam(value = "importDateTo", defaultValue = "") String importDateTo,
		    @RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			LocalDateTime start = DateUtils.stringToLocalDate(importDateFrom, DateUtils.YYYYMMDD_T_HHMMSS);
		    LocalDateTime end = DateUtils.stringToLocalDate(importDateTo, DateUtils.YYYYMMDD_T_HHMMSS);
		    Map<String, Object> result = importService.search(keyword, supplierId, start, end, pageIndex, pageSize);
		    Response<Object> response = new Response<>().success(
					transId,
					Integer.parseInt(result.get("totalItem").toString()),
					result.get("data")
			);
			return new ResponseEntity<>(
					response,
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid ImportModel importModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			importModel.setId("");
			List<Error> errors = importService.validator(importModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			Import importEntity = importService.create(importModel);
			if (Objects.isNull(importModel)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Import>().success(
							transId,
							importEntity
							),
					HttpStatus.CREATED
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> update(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid ImportModel importModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			importModel.setId(id);
			List<Error> errors = importService.validator(importModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			Import importEntity = importService.update(importModel);
			if (Objects.isNull(importEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Import>().success(
							transId,
							importEntity
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
			Error error = importService.delete(id);
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
			ImportDetailResponse response = importService.detail(id);
			return new ResponseEntity<>(
		        new Response<ImportDetailResponse>().success(
		          transId,
		          response
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
