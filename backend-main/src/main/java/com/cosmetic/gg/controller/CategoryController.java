package com.cosmetic.gg.controller;

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
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.validator.ModelValidator;
import com.cosmetic.gg.entity.product.Category;
import com.cosmetic.gg.model.product.CategoryModel;
import com.cosmetic.gg.service.product.CategoryService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/category")
@AllArgsConstructor
@CrossOrigin
public class CategoryController {

	private final CategoryService categoryService;
	
	private final ModelValidator modelValidator;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "parentId", required = false, defaultValue = "") String parentId,
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			Map<String, Object> result = categoryService.search(keyword, parentId, status, pageIndex, pageSize);
			Response<Object> response = new Response<>().success(
			        transId,
			        Integer.parseInt(result.get("totalItem").toString()),
			        result.get("data")
			      );
			
			return new ResponseEntity<>(
			        response,
			        HttpStatus.OK
			      );
		}catch (Exception ex) {
	      log.error(CommonConstant.EXCEPTION, ex.getCause());
	      throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
	    }
	}
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getById(
			@PathVariable(value = "id") String id,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Category category;
		    try {
		    	category = categoryService.getById(id);
		    } catch (Exception ignore) {
		    	category = null;
		    }
		    
		    if (Objects.isNull(category))
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(ErrorCode.NOT_FOUND)),
		          HttpStatus.NOT_FOUND
		        );
		    
		    return new ResponseEntity<>(
		            new Response<CategoryModel>().success(
		              transId,
		              ModelMapper.map(category, CategoryModel.class)
		            ),
		            HttpStatus.OK
		          );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(
			value = "",
			produces = {MediaType.APPLICATION_JSON_VALUE},
		    consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid CategoryModel categoryModel,
		    BindingResult bindingResult,
		    HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			categoryModel.setId("");
			Response<Error> response = modelValidator.validateData(bindingResult, transId);
		      if (response != null)
		          return new ResponseEntity<>(
		            response,
		            HttpStatus.BAD_REQUEST
		          );
		      
		      List<Error> errors = categoryService.validator(categoryModel);
		      if (!errors.isEmpty())
		        return new ResponseEntity<>(
		          new Response<Error[]>().error(
		            transId,
		            errors.toArray(new Error[0])
		          ),
		          HttpStatus.BAD_REQUEST
		        );
		      
		      CategoryModel rs = categoryService.create(categoryModel);
		      if (Objects.isNull(rs)) {
		        ErrorCode errorCode = ErrorCode.FAILURE;
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(errorCode)),
		          HttpStatus.BAD_REQUEST
		        );
		      }
		      
		      return new ResponseEntity<>(
	    	        new Response<CategoryModel>().success(
	    	          transId,
	    	          rs
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
			@RequestBody @Valid CategoryModel categoryModel,
		    BindingResult bindingResult,
		    HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			categoryModel.setId(id);
			Response<Error> response = modelValidator.validateData(bindingResult, transId);
			if (response != null)
		        return new ResponseEntity<>(
		          response,
		          HttpStatus.BAD_REQUEST
		        );
			
			List<Error> errors = categoryService.validator(categoryModel);
		      if (!errors.isEmpty())
		        return new ResponseEntity<>(
		          new Response<Error[]>().error(
		            transId,
		            errors.toArray(new Error[0])
		          ),
		          HttpStatus.BAD_REQUEST
		        );
		      
		      Category contentBefore = categoryService.getById(id);
		      CategoryModel rs = categoryService.update(categoryModel);
		      if (Objects.isNull(rs)) {
		        ErrorCode errorCode = ErrorCode.FAILURE;
		        response = new Response<Error>().error(transId, new Error().builder(errorCode));
		        return new ResponseEntity<>(
		          response,
		          HttpStatus.BAD_REQUEST
		        );
		      }
		      
		      return new ResponseEntity<>(
	    	        new Response<CategoryModel>().success(transId, rs),
	    	        HttpStatus.OK
	    	      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/image/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> changeImage(
			@PathVariable(value = "id") String id,
			@RequestParam("image") MultipartFile imageFile,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			ErrorCode errorCode = categoryService.changeImage(imageFile, id);
			if (errorCode != ErrorCode.SUCCESS) {
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(errorCode)),
		          HttpStatus.BAD_REQUEST
		        );
		      }
			return new ResponseEntity<>(
	            new Response<String>().success(
	              transId,
	              errorCode.getEn()
	            ),
	            HttpStatus.NO_CONTENT
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
			Category contentBefore = categoryService.getById(id);
		    ErrorCode errorCode = categoryService.delete(id);
		    if (errorCode != ErrorCode.SUCCESS) {
		        return new ResponseEntity<>(
		          new Response<Error>().error(transId, new Error().builder(errorCode)),
		          HttpStatus.BAD_REQUEST
		        );
		      }
		    return new ResponseEntity<>(
	            new Response<String>().success(
	              transId,
	              errorCode.getEn()
	            ),
	            HttpStatus.NO_CONTENT
	          );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/children/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> children(
			@PathVariable(value = "id") String id,
		    @RequestParam(value = "all") boolean isAll,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<CategoryModel> categories = categoryService.children(id, isAll);
		      return new ResponseEntity<>(
		        new Response<List<CategoryModel>>().success(
		          transId,
		          categories.size(),
		          categories
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/treeview")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> buildTreeView() {
		String transId = UUID.randomUUID().toString();
		try {
			List<CategoryModel> result = categoryService.buildTreeView();
		      Response<Object> response = new Response<>().success(
		        transId,
		        result.size(),
		        result
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
	
	@GetMapping(value = "/root")
	  @ResponseStatus(value = HttpStatus.OK)
	  public ResponseEntity<Response<?>> root() {
	    String transId = UUID.randomUUID().toString();
	    try {
	      List<CategoryModel> categoryModels = categoryService.root();
	      return new ResponseEntity<>(
	        new Response<List<CategoryModel>>().success(
	          transId,
	          categoryModels.size(),
	          categoryModels
	        ),
	        HttpStatus.OK
	      );
	    } catch (Exception ex) {
	      log.error(CommonConstant.EXCEPTION, ex.getCause());
	      throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
	    }
	  }
}
